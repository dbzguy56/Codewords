{-# LANGUAGE TemplateHaskell #-}

module Common.Protocol where

import Common.Codewords

import Control.Lens
import Data.Aeson.TH
import Data.Text (Text)

data ClientMsg
  = CreateName Text
  | CreateRoom Text (Maybe Password)

data ServerMsg
  = NameCreated User
  | RoomCreated Int Room

deriveJSON defaultOptions ''ClientMsg
deriveJSON defaultOptions ''ServerMsg

makePrisms ''ServerMsg
