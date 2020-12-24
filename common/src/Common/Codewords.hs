{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Codewords where

import Control.Lens
import Data.Aeson.TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List.Index
import Data.List.NonEmpty (NonEmpty)

import System.Random

testWords :: [T.Text]
testWords = ["Apple", "Blue", "Code", "Dart", "Ear", "Fart", "Green"
            , "Horn", "Italy", "Joker", "Kart", "Lift", "Maroon"
            , "Night", "Octopus", "Popcorn", "Queen", "Realtor"
            , "Sweater", "Tree", "Uranium", "Vent", "Water"
            , "Xylophone", "Yeast", "Zoo"]

data User
  = User { name :: T.Text
         , userID :: Int
         }
  deriving Show

data GameState
  = GameState { currentTurn :: Team
              , gameBoard :: Board
              , players :: [Player]
              , winner :: Bool
              }
  deriving Show

data Player
  = Player { role :: PlayerRole
           , team :: Team
           , user :: User
           }
  deriving Show

data PlayerRole
  = Speaker
  | Listener
  deriving Show

data Team
  = Blue
  | Red
  deriving (Bounded, Enum, Eq, Show)

instance Random Team where
  randomR (minTeam, maxTeam) gen = (toEnum a, b)
    where (a, b) = randomR (fromEnum minTeam, fromEnum maxTeam) gen
  random = randomR (minBound, maxBound)


data Ownership
  = TeamOwned Team
  | Bystander
  | Killer
  deriving Show

data Codeword
  = Codeword { word :: T.Text
             , owner :: Ownership
             , revealed :: Bool
             }
  deriving Show

data Clue
  = Clue { hint :: T.Text
         , guessesLeft :: Int
         }

type Board = [Codeword]

data RoomChatMessage
  = RoomChatMessage { userSpeaking :: User
                    , chatMessage :: T.Text
                    }
                    deriving Show

data Room
  = Room { _roomChat :: [RoomChatMessage]
         , _roomName :: T.Text
         , _roomPassword :: Maybe T.Text
         , _roomPlayers :: NonEmpty User
         , _inGame :: Bool
         }
         deriving Show

type Password = T.Text
type Rooms = [Room]

deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''Room
deriveJSON defaultOptions ''RoomChatMessage
deriveJSON defaultOptions ''Player
deriveJSON defaultOptions ''PlayerRole
deriveJSON defaultOptions ''Team

makeLenses ''Room

swapAtIndices :: Int -> Int -> [a] -> [a]
swapAtIndices indexX indexY list = setAt indexY x $ setAt indexX y list
  where x = list !! indexX
        y = list !! indexY

makeCodeword :: Ownership -> T.Text -> Codeword
makeCodeword owner w = Codeword w owner False

makePlayer :: PlayerRole -> Team -> User -> Player
makePlayer role team user = Player role team user

makeNewRoom :: User -> T.Text -> Maybe Password -> Room
makeNewRoom user name pass = Room [] name pass (pure user) False

randomizeList :: Show a => [a] -> IO [a]
randomizeList !board = swapRandom board 99
  where swapRandom b 0 = return b
        swapRandom b n = do
          x <- randomRIO (0, (lengthWords) - 1)
          y <- randomRIO (0, (lengthWords) - 1)
          let temp = swapAtIndices x y b

          swapRandom temp (n - 1)

        lengthWords = length board

assignOwnership :: [T.Text] -> Team -> IO Board
assignOwnership n firstTurn = do
  nouns <- randomizeList n
  let (b, r) = f firstTurn

      blueWords = map (makeCodeword (TeamOwned Blue)) $ take b nouns
      redWords = map (makeCodeword (TeamOwned Red)) $ take r $ drop b nouns
      killerWord = map (makeCodeword Killer) $ take 1 $ drop r $ drop b nouns
      bystaderWords = map (makeCodeword Bystander) $ drop 1 $ drop r $ drop b nouns

      allWords = blueWords ++ redWords ++ killerWord ++ bystaderWords
  return allWords
    where f Blue = (9, 8)
          f Red  = (8, 9)

newGame :: [User] -> IO GameState
newGame p = do
  players <- randomizeList p

  let blueSpeaker = map (makePlayer Speaker Blue) $ take 1 players
      redSpeaker = map (makePlayer Speaker Red) $ take 1 $ drop 1 players
      blueListeners = map (makePlayer Listener Blue)
        $ take (listenersPerTeam) $ drop 2 players
      redListeners = map (makePlayer Listener Red)
        $ take (listenersPerTeam) $ drop listenersPerTeam $ drop 2 players

      allPlayers = blueSpeaker ++ redSpeaker ++ blueListeners ++ redListeners

  firstTurn <- randomIO
  gameBoard <- assignOwnership testWords firstTurn

  return (GameState firstTurn gameBoard allPlayers False)
  where listenersPerTeam =  (div (length p) 2) - 1

revealWords :: GameState -> [T.Text] -> GameState
revealWords gameState guessWords = undefined
