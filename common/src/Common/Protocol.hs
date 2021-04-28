{-# LANGUAGE TemplateHaskell #-}

module Common.Protocol where

import Common.Codewords

import Control.Lens
import Data.Aeson.TH
import Data.IntMap
import Data.Text (Text)

data ClientMsg
  = CreateName Text
  | CreateRoom Text (Maybe Password)
  | ChangeGameState Int Codeword
  | EndTurn Int
  | JoinRoom Int
  | LeaveRoom Int
  | SendClue Int Clue
  | SendRoomChatMsg Int RoomChatMessage
  | StartGame Int
  deriving Show

data ServerMsg
  = GameStarted Int
  | GameStateChanged Int
  | NameCreated User
  | RoomCreated Int Room
  | RoomChanged Int Room
  | RoomDeleted Int
  | RoomJoined Int
  | RoomList (IntMap Room)
  | LeftRoom User

deriveJSON defaultOptions ''ClientMsg
deriveJSON defaultOptions ''ServerMsg

makePrisms ''ServerMsg
