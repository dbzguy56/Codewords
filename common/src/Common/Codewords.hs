{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
            , "Yeast", "Zoo"]

data User
  = User { _name :: T.Text
         , _userID :: Int
         }
  deriving (Eq, Show)

type Board = [Codeword]

data GameState
  = GameState { _currentTurn :: Team
              , _gameBoard :: Board
              , _players :: [Player]
              , _winner :: Bool
              }
  deriving (Eq, Show)

data Player
  = Player { _role :: PlayerRole
           , _team :: Team
           , _user :: User
           }
  deriving (Eq, Show)

data PlayerRole
  = Guesser
  | Listener
  | Speaker
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Codeword
  = Codeword { _word :: T.Text
             , _owner :: Ownership
             , _revealed :: Bool
             }
  deriving (Eq, Show)

data Clue
  = Clue { hint :: T.Text
         , guessesLeft :: Int
         }


data RoomChatMessage
  = RoomChatMessage { userSpeaking :: User
                    , chatMessage :: T.Text
                    }
                    deriving (Eq, Show)

data Room
  = Room { _roomAdmin :: User
         , _roomChat :: [RoomChatMessage]
         , _roomName :: T.Text
         , _roomPassword :: Maybe T.Text
         , _roomPlayers :: NonEmpty User
         , _roomGameState :: Maybe GameState
         }
         deriving (Eq, Show)

type Password = T.Text
type Rooms = [Room]

deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''Room
deriveJSON defaultOptions ''RoomChatMessage
deriveJSON defaultOptions ''Player
deriveJSON defaultOptions ''PlayerRole
deriveJSON defaultOptions ''Team
deriveJSON defaultOptions ''GameState
deriveJSON defaultOptions ''Codeword
deriveJSON defaultOptions ''Ownership

makeLenses ''Codeword
makeLenses ''GameState
makeLenses ''Player
makeLenses ''PlayerRole
makeLenses ''Room
makeLenses ''User

tryStartGame :: User -> GameState -> Room -> Room
tryStartGame u gs r@Room{..}
  | isAdmin = r {_roomGameState = Just gs}
  | otherwise = r
  where isAdmin = _userID u == _userID _roomAdmin


swapAtIndices :: Int -> Int -> [a] -> [a]
swapAtIndices indexX indexY list = setAt indexY x $ setAt indexX y list
  where x = list !! indexX
        y = list !! indexY

makeCodeword :: Ownership -> T.Text -> Codeword
makeCodeword owner w = Codeword w owner False

makePlayer :: PlayerRole -> Team -> User -> Player
makePlayer role team user = Player role team user

makeNewRoom :: User -> T.Text -> Maybe Password -> Room
makeNewRoom user name pass = Room user [] name pass (pure user) Nothing

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
  allWords' <- randomizeList allWords
  return allWords'
    where f Blue = (9, 8)
          f Red  = (8, 9)

startNewGame :: [User] -> IO GameState
startNewGame p = do
  players <- randomizeList p

  let blueGuesser = map (makePlayer Guesser Blue) $ take 1 players
      redGuesser = map (makePlayer Guesser Red) $ take 1 $ drop 1 players
      blueSpeaker = map (makePlayer Speaker Blue) $ take 1 $ drop 2 players
      redSpeaker = map (makePlayer Speaker Red) $ take 1 $ drop 3 players
      blueListeners = map (makePlayer Listener Blue)
        $ take (listenersPerTeam) $ drop 4 players
      redListeners = map (makePlayer Listener Red)
        $ takeWhile (const True) $ drop listenersPerTeam $ drop 4 players

      allPlayers = blueGuesser ++ redGuesser
        ++ blueSpeaker ++ redSpeaker
        ++ blueListeners ++ redListeners

  firstTurn <- randomIO
  gameBoard <- assignOwnership testWords firstTurn

  players' <- randomizeList allPlayers
  return (GameState firstTurn gameBoard players' False)
  where listenersPerTeam = (div (length p) 2) - 2

revealWords :: GameState -> [T.Text] -> GameState
revealWords gameState guessWords = undefined
