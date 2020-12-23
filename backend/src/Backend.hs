{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Backend where

import Common.Codewords
import Common.Protocol
import Common.Route
import Control.Concurrent
import Obelisk.Backend
import Obelisk.Route hiding (decode, encode)
import Safe

import Data.Aeson
import Data.IntMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.WebSockets
import Network.WebSockets.Snap


data ServerState
  = State { userIDCounter :: MVar Int
          , rooms :: MVar (IntMap Room)
          }


handleNewConnection :: Maybe User -> ServerState -> Connection -> IO ()
handleNewConnection mUser state connection = do
  let send = sendTextData connection.encode
  a <- receiveData connection
  case decode a of
    Just decodedMsg ->
      case decodedMsg of
        CreateName name -> do
          newID <- nextUserID (userIDCounter state)
          let newUser = User name newID
          send (NameCreated newUser)
          handleNewConnection (Just newUser) state connection

        CreateRoom name pass -> do
          case mUser of
            Just u -> do
              let newRoom = makeNewRoom u name pass
              roomID <- modifyMVar (rooms state) (return.addRoom newRoom)
              send (RoomCreated roomID newRoom)
            Nothing ->
              return ()

          handleNewConnection mUser state connection

    _  ->
      handleNewConnection mUser state connection

handleWebsocket :: ServerState -> PendingConnection -> IO ()
handleWebsocket s p = do
  fullConnection <- acceptRequest p
  handleNewConnection Nothing s fullConnection
  --sendTextData fullConnection ("Oof" :: T.Text)
  --sendClose fullConnection ("You are already dead" :: T.Text)
  return ()

nextUserID :: MVar Int -> IO Int
nextUserID a = modifyMVar a $ \c -> do
  return $ (c + 1, c)

addRoom :: Room -> IntMap Room -> (IntMap Room, Int)
addRoom r m = (insert newIndex r m, newIndex)
  where biggestIndex = maybe 0 id $ maximumMay $ keys m
        newIndex = biggestIndex + 1

initServer :: IO ServerState
initServer = do
  c <- newMVar (0 :: Int)
  rooms <- newMVar mempty
  return $ State c rooms

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
