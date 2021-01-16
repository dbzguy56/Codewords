{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.List.NonEmpty as NE ((<|), filter, fromList, nonEmpty, toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.WebSockets
import Network.WebSockets.Snap


data ServerState
  = State { userIDCounter :: TVar Int
          , rooms :: TVar (IntMap Room)
          , broadcast :: TChan ServerMsg
          }

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
handleClientMsg state mUser cMsg send =
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
          mRoom <- appendUserToRoom (rooms state) roomID u
          case mRoom of
            Just r -> do
              broadcastMsgToRoom (rooms state) roomID state $
                RoomChatMessage systemUser (
                  (name u) <> " has joined the room."
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
          mRoom <- removeUserFromRoom (rooms state) roomID u
          case mRoom of
            Just r -> do
              broadcastMsgToRoom (rooms state) roomID state $
                RoomChatMessage systemUser (
                  (name u) <> " has left the room."
                )
            Nothing ->
              atomically $ writeTChan (broadcast state) (RoomDeleted roomID)
          send (LeftRoom u)
        Nothing ->
          return ()
      return mUser

    SendRoomChatMsg roomID roomChatMsg -> do
      broadcastMsgToRoom (rooms state) roomID state roomChatMsg
      return mUser

    StartGame roomID -> do
      case mUser of
        Just u -> do
          let gs = GameState Blue [] [] False
          mRoom <- tryFnOnRoom (rooms state) roomID
            (tryStartGame u gs)

          case mRoom of
            Just r -> do
              broadcastMsgToRoom (rooms state) roomID state $
                RoomChatMessage systemUser $
                  (name u) <> " has started the game."

              newGame <- startNewGame $ NE.toList $ _roomPlayers r

              return ()

            Nothing ->
              return ()
        Nothing ->
          return ()
      return mUser

tryFnOnRoom :: TVar (IntMap Room) -> Int
  -> (Room -> Room) -> IO (Maybe Room)
tryFnOnRoom rList rID fn = do
    roomList <- readTVarIO rList
    roomExist $ M.lookup rID roomList
  where roomExist Nothing = return Nothing
        roomExist (Just _) = atomically $ stateTVar rList $ \r ->
          let r' = adjust fn rID r in
          (M.lookup rID r' , r')


handleTChanComms :: Connection -> TChan ServerMsg -> IO ()
handleTChanComms c t = forever $ do
  let send = sendTextData c.encode
  s <- atomically $ readTChan t
  send s

appendUserToRoom :: TVar (IntMap Room) -> Int -> User
  -> IO (Maybe Room)
appendUserToRoom rList rID u = atomically $ stateTVar rList $ \r ->
  let r' = adjust addUser rID r in
  (M.lookup rID r', r')
  where addUser = over roomPlayers (u <|)

removeUserFromRoom :: TVar (IntMap Room) -> Int -> User
  -> IO (Maybe Room)
removeUserFromRoom rList rID u = atomically $ stateTVar rList $ \r ->
  let
      oldMembers = fmap _roomPlayers $ M.lookup rID r
      newMembers = (NE.nonEmpty . NE.filter (\newU -> userID u /= userID newU )) =<< oldMembers
      r' =
        case newMembers of
          Just nm -> adjust (set roomPlayers nm) rID r
          Nothing -> delete rID r
  in
  (M.lookup rID r', r')

appendRoomChatMsg :: TVar (IntMap Room) -> Int
  -> RoomChatMessage -> IO (Maybe Room)
appendRoomChatMsg rList rID rChatMsg = atomically $ stateTVar rList $ \r ->
  let r' = adjust addMsg rID r in
  (M.lookup rID r', r')
  where addMsg = over roomChat (rChatMsg :)  -- room {_roomChat = rChatMsg : _roomChat room}

broadcastMsgToRoom :: TVar (IntMap Room) -> Int
  -> ServerState -> RoomChatMessage -> IO ()
broadcastMsgToRoom rList rID state rChatMsg = do
  mRoom <- appendRoomChatMsg rList rID rChatMsg
  case mRoom of
    Just r ->
      atomically $ writeTChan (broadcast state) (RoomChanged rID r)
    Nothing ->
      return ()

nextUserID :: TVar Int -> IO Int
nextUserID a = atomically $ stateTVar a $ \c -> (c, c + 1)

addRoom :: Room -> IntMap Room -> (Int, IntMap Room)
addRoom r m = (newIndex, insert newIndex r m)
  where biggestIndex = maybe 0 id $ maximumMay $ keys m
        newIndex = biggestIndex + 1

initServer :: IO ServerState
initServer = do
  c <- newTVarIO (0 :: Int)
  rooms <- newTVarIO mempty
  broadcast <- newBroadcastTChanIO
  return $ State c rooms broadcast

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
