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
import Data.List as L (filter)
import qualified Data.List.NonEmpty as NE
  ((<|), filter, length, nonEmpty, toList, last)
import qualified Data.Text as T

import Network.WebSockets
import Network.WebSockets.Snap

import System.Random

data UConnection
  = UConn { uID :: UserID
          , connection :: Connection
          }

instance Eq UConnection where
  (==) (UConn a _) (UConn b _) = a == b

data ServerState
  = State { userIDCounter :: TVar Int
          , rooms :: TVar (IntMap Room)
          , broadcast :: TChan ServerMsg
          , usersConnected :: TVar [UConnection]
          , pureGen :: TVar StdGen
          }

systemUser :: User
systemUser = User ("SYSTEM" :: T.Text) (UserID (-1))

handleNewConnection :: Maybe User -> ServerState -> Connection -> IO ()

handleNewConnection mUser state connection = do
  let s = sendTextData connection.encode
  a <- receiveData connection
  case decode a of
    Just decodedMsg -> do
      newMUser <- handleClientMsg state mUser decodedMsg s connection
      handleNewConnection newMUser state connection

    _  ->
      handleNewConnection mUser state connection

handleWebsocket :: ServerState -> PendingConnection -> IO ()
handleWebsocket s p = do
  fullConnection <- acceptRequest p
  let sendFn = sendTextData fullConnection.encode
  bc <- atomically $ dupTChan (broadcast s)
  forkIO $ handleTChanComms fullConnection bc
  roomIntMap <- atomically $ readTVar (rooms s)
  sendFn (RoomList roomIntMap)
  handleNewConnection Nothing s fullConnection
  return ()


handleClientMsg :: ServerState -> Maybe User -> ClientMsg
  -> (ServerMsg -> IO ()) -> Connection -> IO (Maybe User)
handleClientMsg state mUser cMsg sendFn connection = do

  let updateFn = tryUpdateRoom (rooms state) (broadcast state)

  case cMsg of
    CreateName n -> do
      newID <- nextUserID (userIDCounter state)
      let newUser = User n (UserID newID)
      atomically $ modifyTVar' (usersConnected state) $ \list ->
        (UConn (UserID newID) connection) : list
      sendFn (NameCreated newUser)
      return $ Just newUser

    CreateRoom n pass -> do
      case mUser of
        Just u -> do
          let newRoom = makeNewRoom u n pass
          roomID <- atomically $ stateTVar (rooms state) (addRoom newRoom)
          atomically $ writeTChan (broadcast state) (RoomChanged roomID newRoom)
          sendFn (RoomCreated roomID newRoom)
        Nothing ->
          return ()
      return mUser

    JoinRoom roomID -> do

      case mUser of
        Just u -> do
          mRoom <- updateFn roomID $ appendUserToRoom u

          case mRoom of
            Just _ -> do
              updateFn roomID $ appendRoomChatMsg $ RoomChatMessage systemUser (
                  (_name u) <> " has joined the room."
                )
              sendFn (RoomJoined roomID)
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
            Just _ -> do
              updateFn roomID $ appendRoomChatMsg $ RoomChatMessage systemUser (
                  (_name u) <> " has left the room."
                )
              return ()
            Nothing ->
              return ()
          sendFn (LeftRoom u)
        Nothing ->
          return ()

      return mUser

    SendRoomChatMsg roomID roomChatMsg -> do
      updateFn roomID $ appendRoomChatMsg roomChatMsg
      return mUser

    StartGame roomID -> do
      case mUser of
        Just u -> do
          mRoom <- roomExist (rooms state) roomID
          case (enoughPlayers mRoom) of
            True -> do
              mRoom' <- updateFn roomID $ appendRoomChatMsg $
                RoomChatMessage systemUser $
                  (_name u) <> " has started the game."

              case mRoom' of
                Just r -> do
                  let rPlayers = NE.toList $ _roomPlayers r

                  newGameState <- startNewGame rPlayers
                  mNewRoom <- updateFn roomID $ updateRoomGameState newGameState

                  case mNewRoom of
                    Just _ -> do
                      usConnected <- readTVarIO (usersConnected state)
                      sendUsersServerMsg rPlayers (GameStarted roomID)
                        usConnected

                    Nothing ->
                      return ()

                Nothing ->
                  return ()

            False -> do
              updateFn roomID $ appendRoomChatMsg $
                RoomChatMessage systemUser $ "You need at \
                \ least 4 players to start the game!"
              return ()
        Nothing ->
          return ()

      return mUser

    ChangeGameState roomID codeword -> do
      mRoom <- roomExist (rooms state) roomID
      case mRoom of
        Just r ->
          case (_roomGameState r) of
            Just gs -> do
              case (ifGuessingPhase (_winner gs) $ _turnPhase gs) of
                True -> revealCIfNotAlr codeword gs roomID state
                False -> return ()
            Nothing -> return ()
        Nothing -> return ()

      return mUser

    SendClue roomID clue -> do
      mRoom <- roomExist (rooms state) roomID
      case mRoom of
        Just r ->
          case (_roomGameState r) of
            Just gs -> do
              updateFn roomID $
                updateRoomGameState (gs {_turnPhase = Guessing clue})
              return ()
            Nothing -> return ()
        Nothing -> return ()

      return mUser

    EndTurn roomID -> do
      mRoom <- roomExist (rooms state) roomID
      case mRoom of
        Just r ->
          case (_roomGameState r) of
            Just gs -> do
              case (ifGuessingPhase (_winner gs) $ _turnPhase gs) of
                True -> do
                  newGS <- atomically $ stateTVar (pureGen state) $ \pGen ->
                    newTurn gs pGen

                  updateFn roomID $ updateRoomGameState newGS
                  return ()
                False -> return ()
            Nothing -> return ()
        Nothing -> return ()

      return mUser

    EndGame roomID -> do
      mRoom <- roomExist (rooms state) roomID
      case mRoom of
        Just _ -> do
          newR <- updateFn roomID $ changeToLobby

          case newR of
            Just r' -> do
              usConnected <- readTVarIO (usersConnected state)

              let rPlayers = NE.toList $ _roomPlayers r'
              sendUsersServerMsg rPlayers (GameEnded roomID)
                usConnected

              sendUsersServerMsg rPlayers (RoomChanged roomID r')
                usConnected
              return ()

            Nothing -> return ()

        Nothing -> return ()
      return mUser

    ChangeRoomName roomID newRoomName -> do
      mRoom <- roomExist (rooms state) roomID
      case mRoom of
        Just _ -> do
          updateFn roomID $ changeRoomName newRoomName
          return ()
        Nothing -> return ()
      return mUser

ifGuessingPhase :: Maybe Team -> TurnPhase -> Bool
ifGuessingPhase Nothing (Guessing _) = True
ifGuessingPhase _ _ = False

enoughPlayers :: Maybe (Room) -> Bool
enoughPlayers (Just r)
  | (NE.length $ _roomPlayers r) > 3 = True
  | otherwise = False
enoughPlayers _ = False

revealCIfNotAlr :: Codeword -> GameState -> Int -> ServerState
  -> IO ()
revealCIfNotAlr c@(Codeword _ _ False) gs roomID state = do
  let updateFn = tryUpdateRoom (rooms state) (broadcast state)

  newGameState <- atomically $ stateTVar (pureGen state) $ \pGen ->
    revealCodeword c gs pGen

  updateFn roomID $ updateRoomGameState newGameState
  return ()
revealCIfNotAlr _ _ _ _ = return ()


sendUsersServerMsg :: [User] -> ServerMsg
  -> [UConnection] -> IO ()
sendUsersServerMsg roomUsers sMsg userCs = do
  let uCs = L.filter (roomContainsUC roomUsers) userCs
  sendMsgs uCs
  where roomContainsUC :: [User] -> UConnection
          -> Bool
        roomContainsUC [] _ = False
        roomContainsUC (u:us) uC
          | (_userID u) == (uID uC) = True
          | otherwise = roomContainsUC us uC
        sendMsgs [] = return ()
        sendMsgs (x:xs) = do
          let s = sendTextData (connection x).encode
          s sMsg
          sendMsgs xs


tryUpdateRoom :: TVar (IntMap Room) -> TChan ServerMsg
  -> Int -> (Room -> Room) -> IO (Maybe Room)

tryUpdateRoom rTVar broadcastChan rID fn = do
  r <- roomExist rTVar rID
  case r of
    Just _ -> do
      mRoom <- atomically $ stateTVar rTVar $ \rMap ->
        let r' = adjust fn rID rMap in
        (M.lookup rID r', r')

      case mRoom of
        Just room ->
          atomically $ writeTChan broadcastChan (RoomChanged rID room)
        Nothing -> do
          putStrLn "Could not apply fn to update room."
          return ()
      return mRoom

    Nothing -> do
      putStrLn "Room does not exist, could not update room."
      return Nothing

roomExist :: TVar (IntMap Room) -> Int
  -> IO (Maybe Room)
roomExist rTVar rID = do
  rIntMap <- readTVarIO rTVar
  return $ M.lookup rID rIntMap


deleteRoom :: TVar (IntMap Room) -> Int
  -> IO (Maybe Room)
deleteRoom rTVar rID = atomically $ stateTVar rTVar $ \r ->
  let r' = M.delete rID r in
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
  where f 1 = do
          deleteRoom rTVar rID
          atomically $ writeTChan broadcastChan (RoomDeleted rID)
          return Nothing
        f _ = tryUpdateRoom rTVar broadcastChan rID fn

handleTChanComms :: Connection -> TChan ServerMsg
  -> IO ()
handleTChanComms c t = forever $ do
  let sendFn = sendTextData c.encode
  s <- atomically $ readTChan t
  sendFn s


appendUserToRoom :: User -> Room
  -> Room
appendUserToRoom u r = addUser r
  where addUser = over roomPlayers (u NE.<|)

removeUserFromRoom :: User -> Room
  -> Room
removeUserFromRoom u r = do

  let oldMembers = _roomPlayers r
      newMembers = NE.nonEmpty $ NE.filter
        (\newU -> _userID u /= _userID newU) oldMembers
  case newMembers of
    Just p -> do
      let newR = r {_roomPlayers = p}
      newR {_roomAdminID = _userID $ NE.last p}
    Nothing ->
      r

updateRoomGameState :: GameState -> Room
  -> Room
updateRoomGameState gs r@Room{..} = r {_roomGameState = Just gs}

changeToLobby :: Room -> Room
changeToLobby r@Room{..} = r {_roomGameState = Nothing}

changeRoomName :: T.Text -> Room
  -> Room
changeRoomName newName r@Room{..} = r {_roomName = newName}

appendRoomChatMsg :: RoomChatMessage -> Room -> Room
appendRoomChatMsg rChatMsg r = addMsg r
  where addMsg = over roomChat (rChatMsg :)


nextUserID :: TVar Int -> IO Int
nextUserID a = atomically $ stateTVar a $ \c -> (c, c + 1)

addRoom :: Room -> IntMap Room -> (Int, IntMap Room)
addRoom r m = (newIndex, M.insert newIndex r m)

  where biggestIndex = maybe 0 id $ maximumMay $ keys m
        newIndex = biggestIndex + 1

initServer :: IO ServerState
initServer = do
  c <- newTVarIO (0 :: Int)
  rooms <- newTVarIO mempty
  uConnections <- newTVarIO mempty
  broadcast <- newBroadcastTChanIO
  pGen <- newTVarIO $ mkStdGen 137
  return $ State c rooms broadcast uConnections pGen

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    s <- initServer
    serve $ \case
      BackendRoute_Websocket :/ () ->
          runWebSocketsSnap $ handleWebsocket s

      _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
