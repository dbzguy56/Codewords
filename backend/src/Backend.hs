{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import Common.Codewords
import Control.Lens hiding ((<|))
import Common.Protocol
import Common.Route
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Obelisk.Backend
import Obelisk.Route hiding (decode, encode)
import Safe

import Data.Aeson
import Data.IntMap as M
import Data.List.NonEmpty as NE ((<|), filter, fromList, length, nonEmpty, toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.WebSockets
import Network.WebSockets.Snap


data ServerState
  = State { userIDCounter :: TVar Int
          , rooms :: TVar (IntMap Room)
          , broadcast :: TChan ServerMsg
          }
{-
  TVar a
    Shared memory locations that support atomic memory transactions.

  TChan a
    TChan is an abstract type representing an unbounded FIFO channel.
-}


systemUser = User ("SYSTEM" :: T.Text) (-1)

handleNewConnection :: Maybe User -> ServerState -> Connection -> IO ()
handleNewConnection mUser state connection = do
  let send = sendTextData connection.encode
  a <- receiveData connection
  case decode a of
    Just decodedMsg -> do
      newMUser <- handleClientMsg state mUser decodedMsg send
      handleNewConnection newMUser state connection

    _  ->
      handleNewConnection mUser state connection

handleWebsocket :: ServerState -> PendingConnection -> IO ()
handleWebsocket s p = do
    {-
      PendingConnection
        A new client connected to the server. We haven't
        accepted the connection yet, though.

      sendTextData :: WebSocketsData a => Connection -> a -> IO ()
        Send a textual message. The message will be
        encoded as UTF-8. This should be the default choice
        for human-readable text-based protocols such as
        JSON.

      dupTChan :: TChan a -> STM (TChan a)
        Duplicate a TChan: the duplicate channel begins empty,
        but data written to either channel from then on will
        be available from both. Hence this creates a kind of
        broadcast channel, where data written by anyone is
        seen by everyone else.

      ThreadId
        A ThreadId is an abstract type representing a handle
        to a thread. ThreadId is an instance of Eq, Ord and
        Show, where the Ord instance implements an
        arbitrary total ordering over ThreadIds. The Show
        instance lets you convert an arbitrary-valued
        ThreadId to string form; showing a ThreadId value is
        occasionally useful when debugging or diagnosing the
        behaviour of a concurrent program.


      forkIO :: IO () -> IO ThreadId
        Creates a new thread to run the IO computation passed
        as the first argument, and returns the ThreadId of
        the newly created thread. The new thread will be a
        lightweight, unbound thread.


      readTVar :: TVar a -> STM a
      Return the current value stored in a TVar.
    -}
  fullConnection <- acceptRequest p
  let send = sendTextData fullConnection.encode
  bc <- atomically $ dupTChan (broadcast s)
  forkIO $ handleTChanComms fullConnection bc
  roomIntMap <- atomically $ readTVar (rooms s)
  send (RoomList roomIntMap)
  handleNewConnection Nothing s fullConnection
  --sendTextData fullConnection ("Oof" :: T.Text)
  --sendClose fullConnection ("You are already dead" :: T.Text)
  return ()


handleClientMsg :: ServerState -> Maybe User -> ClientMsg
  -> (ServerMsg -> IO ()) -> IO (Maybe User)
handleClientMsg state mUser cMsg send = do
  {-
  writeTVar :: TVar a -> a -> STM ()
    Write the supplied value into a TVar.
  -}
  let updateFn = tryUpdateRoom (rooms state) (broadcast state)

  case cMsg of
    CreateName name -> do
      newID <- nextUserID (userIDCounter state)
      let newUser = User name newID
      send (NameCreated newUser)
      return $ Just newUser

    CreateRoom name pass -> do
      case mUser of
        Just u -> do
          let newRoom = makeNewRoom u name pass
          roomID <- atomically $ stateTVar (rooms state) (addRoom newRoom)
          atomically $ writeTChan (broadcast state) (RoomChanged roomID newRoom)
          send (RoomCreated roomID newRoom)
        Nothing ->
          return ()
      return mUser

    JoinRoom roomID -> do

      case mUser of
        Just u -> do
          mRoom <- updateFn roomID $ appendUserToRoom u

          case mRoom of
            Just r -> do
              updateFn roomID $ appendRoomChatMsg $ RoomChatMessage systemUser (
                  (_name u) <> " has joined the room."
                )
              send (RoomJoined roomID)
            Nothing ->
              return ()
        Nothing ->
          return ()

      return mUser

    LeaveRoom roomID -> do

      case mUser of
        Just u -> do
          mRoom <- removeUserOrRoom (rooms state) (broadcast state) roomID $
            removeUserFromRoom u

          case mRoom of
            Just r -> do
              updateFn roomID $ appendRoomChatMsg $ RoomChatMessage systemUser (
                  (_name u) <> " has left the room."
                )
              return ()
              -- TODO: Delete Room after no users

            Nothing ->
              atomically $ writeTChan (broadcast state) (RoomDeleted roomID)
          send (LeftRoom u)
        Nothing ->
          return ()

      return mUser

    SendRoomChatMsg roomID roomChatMsg -> do
      updateFn roomID $ appendRoomChatMsg roomChatMsg
      return mUser

    StartGame roomID -> do
      case mUser of
        Just u -> do
          let gs = GameState Blue [] [] False
          mRoom <- updateFn roomID $ tryStartGame u gs

          case mRoom of
            Just r -> do
              updateFn roomID $ appendRoomChatMsg $
                RoomChatMessage systemUser $
                  (_name u) <> " has started the game."

              newGameState <- startNewGame $ NE.toList $ _roomPlayers r

              mNewRoom <- updateFn roomID $ updateRoomGameState newGameState
              case mNewRoom of
                Just newR -> do
                  atomically $ writeTChan (broadcast state) (GameStarted roomID)
                  atomically $ writeTChan (broadcast state) (RoomChanged roomID newR)

                Nothing ->
                  return ()
            --TODO: clean these up using monad?
            Nothing ->
              return ()
        Nothing ->
          return ()

      return mUser


tryUpdateRoom :: TVar (IntMap Room) -> TChan ServerMsg
  -> Int -> (Room -> Room) -> IO (Maybe Room)
  {-
    stateTvar :: TVar s -> (s -> (a, s)) -> STM a
      Like modifyTVar' (mutates content) but the function is a
      simple state transition that can return a side value which
      is passed on as the result of the STM.

    STM a
      A monad supporting atomic memory transactions.

    readTVarIO :: TVar a -> IO a
      Return the current value stored in a TVar. This is equivalent to
        readTVarIO = atomically . readTVar

    atomically :: STM a -> IO a
      Performs a series of STM actions atomically.

    adjust :: (a -> a) -> Key -> IntMap a -> IntMap a

    set :: ASetter s t a b -> b -> s -> t

    lookup :: Key -> IntMap a -> Maybe a
  -}
tryUpdateRoom rTVar broadcastChan rID fn  = do
  rIntMap <- readTVarIO rTVar
  roomExist $ M.lookup rID rIntMap
  where roomExist Nothing = return Nothing
        roomExist (Just _) = do
          mRoom <- atomically $ stateTVar rTVar $ \rMap ->
            let r' = adjust fn rID rMap in
            (M.lookup rID r', r')

          case mRoom of
            Just r ->
              atomically $ writeTChan broadcastChan (RoomChanged rID r)
            Nothing -> return ()
          return mRoom
{-
  atomically $ stateTVar rTVar $ \rMap ->
  let
    mRoom = M.lookup rID rMap
    r' =
      case mRoom of
        Just r -> fn r
        Nothing -> rMap
  in
  (M.lookup rID r', r')
-}

deleteRoom :: TVar (IntMap Room) -> Int
  -> IO (Maybe Room)
deleteRoom rTVar rID = atomically $ stateTVar rTVar $ \r ->
  let r' = delete rID r in
  (M.lookup rID r', r')

removeUserOrRoom :: TVar (IntMap Room) -> TChan ServerMsg
  -> Int -> (Room -> Room) -> IO (Maybe Room)
removeUserOrRoom rTVar broadcastChan rID fn = do
  rIntMap <- readTVarIO rTVar
  let mRoom = M.lookup rID rIntMap
  case mRoom of
    Just r ->
      f (NE.length $ _roomPlayers r)
    Nothing ->
      return Nothing
  where f 1 = deleteRoom rTVar rID
        f _ = tryUpdateRoom rTVar broadcastChan rID fn

handleTChanComms :: Connection -> TChan ServerMsg -> IO ()
handleTChanComms c t = forever $ do
  {-
  class Functor f => Applicative f where
    A functor with application, providing operations to
      i) embed pure expressions (pure), and
      ii) sequence computations and combine their results
          (<*> and liftA2).

  forever :: Applicative f => f a -> f b
    Repeat an action indefinitely.
  -}
  let send = sendTextData c.encode
  s <- atomically $ readTChan t
  send s

appendUserToRoom :: User -> Room
  -> Room
  {-
  over :: ASetter s t a b -> (a -> b) -> s -> t
    Modify the target of a Lens or all the targets of a
    Setter or Traversal with a function.

  (<|) :: a -> NonEmpty a -> NonEmpty a
    Prepend an element to the stream.

  -}
appendUserToRoom u r = addUser r
  where addUser = over roomPlayers (u <|)

removeUserFromRoom :: User -> Room
  -> Room
removeUserFromRoom u r = do
  {-
  (=<<) :: Monad m => (a -> m b) -> m a -> m b
    Sequentially compose two actions, passing any value
    produced by the second as an argument to the first.
      'bs =<< as' can be understood as the do expression
        do a <- as
          bs a

  filter :: (a -> Bool) -> NonEmpty a -> [a]

  nonEmpty :: [a] -> Maybe (NonEmpty a)

  fmapMaybe :: Filterable f => (a -> Maybe b)
    -> f a -> f b
  -}
  let oldMembers = _roomPlayers r
      newMembers = NE.nonEmpty $ NE.filter (\newU -> _userID u /= _userID newU) oldMembers
  case newMembers of
    Just p ->
      r {_roomPlayers = p}
    Nothing ->
      r

updateRoomGameState :: GameState -> Room
  -> Room
updateRoomGameState gs r@Room{..} = r {_roomGameState = Just gs}



appendRoomChatMsg :: RoomChatMessage -> Room -> Room
appendRoomChatMsg rChatMsg r = addMsg r
  where addMsg = over roomChat (rChatMsg :)  -- room {_roomChat = rChatMsg : _roomChat room}


nextUserID :: TVar Int -> IO Int
nextUserID a = atomically $ stateTVar a $ \c -> (c, c + 1)

addRoom :: Room -> IntMap Room -> (Int, IntMap Room)
addRoom r m = (newIndex, insert newIndex r m)
  {-
  maybe :: b -> (a -> b) -> Maybe a -> b
    The maybe function takes a default value, a function, and a
    Maybe value. If the Maybe value is Nothing, the function
    returns the default value. Otherwise, it applies the
    function to the value inside the Just and returns the result.

  maximumMay :: Ord a => [a] -> Maybe a

  insert :: Key -> a -> IntMap a -> IntMap a
    O(min(n,W)). Insert a new key/value pair in the map. If the
    key is already present in the map, the associated value is
    replaced with the supplied value, i.e. insert is equivalent to
    insertWith const.

  keys :: IntMap a -> [Key]
    O(n). Return all keys of the map in ascending order. Subject
    to list fusion.
  -}
  where biggestIndex = maybe 0 id $ maximumMay $ keys m
        newIndex = biggestIndex + 1

initServer :: IO ServerState
initServer = do
  {-
  newTVarIO :: a -> IO (TVar a)
    IO version of newTVar. This is useful for creating top-level
    TVars using unsafePerformIO, because using atomically inside
    unsafePerformIO isn't possible.

  mempty :: a
    Identity of mappend
    mappend :: a -> a -> a
      An associative operation
      (<>) :: a -> a -> a
        [1,2,3] <> [4,5,6]

  newBroadcastTChanIO :: IO (TChan a)
    IO version of newBroadcastTChan.
    newBroadcastTChan :: STM (TChan a)
    Create a write-only TChan. More precisely, readTChan will
    retry even after items have been written to the channel.
    The only way to read a broadcast channel is to duplicate
    it with dupTChan.

    Consider a server that broadcasts messages to clients:
      serve :: TChan Message -> Client -> IO loop
      serve broadcastChan client = do
        myChan <- dupTChan broadcastChan
        forever $ do
            message <- readTChan myChan
            send client message

    The problem with using newTChan to create the broadcast
    channel is that if it is only written to and never read,
    items will pile up in memory. By using newBroadcastTChan
    to create the broadcast channel, items can be garbage
    collected after clients have seen them.
  -}
  c <- newTVarIO (0 :: Int)
  rooms <- newTVarIO mempty
  broadcast <- newBroadcastTChanIO
  return $ State c rooms broadcast

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    {-
    runWebSocketsSnap :: MonadSnap m => ServerApp -> m ()
      The following function escapes from the current Snap
      handler, and continues processing the WebSockets action.
      The action to be executed takes the Request as a
      parameter, because snap has already read this from
      the socket.
    -}
    s <- initServer
    serve $ \case
      BackendRoute_Websocket :/ () ->
          runWebSocketsSnap $ handleWebsocket s

      _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
