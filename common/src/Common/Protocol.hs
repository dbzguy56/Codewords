{-# LANGUAGE TemplateHaskell #-}

module Common.Protocol where

import Common.Codewords

import Control.Lens
import Data.Aeson.TH
import Data.IntMap

data ClientMsg
  = CreateName NonEmptyText
  | CreateRoom NonEmptyText (Maybe NonEmptyText)
  | ChangeRoomName Int NonEmptyText
  | ChangeGameState Int Codeword
  | EndGame Int
  | EndTurn Int
  | JoinRoom Int (Maybe NonEmptyText)
  | LeaveRoom Int
  | SendClue Int Clue
  | SendRoomChatMsg Int RoomChatMessage
  | StartGame Int
  deriving Show

data ServerMsg
  = GameStarted Int
  | GameStateChanged Int
  | GameEnded Int
  | NameCreated User
  | PasswordInvalid Int
  | RoomCreated Int ClientRoom
  | RoomChanged Int ClientRoom
  | RoomDeleted Int
  | RoomJoined Int
  | RoomList (IntMap ClientRoom)
  | LeftRoom User

deriveJSON defaultOptions ''ClientMsg
deriveJSON defaultOptions ''ServerMsg

makePrisms ''ServerMsg
