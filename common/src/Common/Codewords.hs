{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Codewords where

import Control.Lens
import Data.Aeson.TH
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import qualified Data.Text as T
import Data.List.Index
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, lookup, elems)

import System.Random

testWords :: [T.Text]
testWords = ["Apple", "Blue", "Code", "Dart", "Ear", "Fart", "Green"
            , "Horn", "Italy", "Joker", "Kart", "Lift", "Maroon"
            , "Night", "Octopus", "Popcorn", "Queen", "Realtor"
            , "Sweater", "Tree", "Uranium", "Vent", "Water"
            , "Yeast", "Zoo"]

data User
  = User { _name :: T.Text
         , _userID :: UserID
         }
  deriving (Eq, Show)

instance Eq UserID where
  (==) (UserID a) (UserID b) = a == b

instance Show UserID where
  show (UserID a) = show a

instance Ord UserID where
  (<=) (UserID a) (UserID a') = (<=) a a'

instance FromJSONKey UserID

instance ToJSONKey UserID

data CardsLeft
  = CardsLeft { _blueCards :: Int
              , _redCards :: Int
              }
  deriving (Eq, Show)

data GameState
  = GameState { _cardsLeft :: CardsLeft
              , _currentTurn :: Team
              , _gameBoard :: Board
              , _players :: Map UserID Player
              , _turnPhase :: TurnPhase
              , _winner :: Bool
              }
  deriving (Eq, Show)

data TurnPhase
  = CluePicking
  | Guessing Clue
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
  = Clue { _hint :: T.Text
         , _relatedWords :: Int
         , _guessesLeft :: Int
         }

instance Eq Clue where
  (==) (Clue h _ _) (Clue h' _ _) = h == h'

instance Show Clue where
  show (Clue h _ g) = "Current clue: " ++ (show h)
    ++ ", Guesses left: " ++ (show g)

data RoomChatMessage
  = RoomChatMessage { userSpeaking :: User
                    , chatMessage :: T.Text
                    }
                    deriving (Eq, Show)

--TODO: Limit what the front end can see
data Room
  = Room { _roomAdmin :: User
         , _roomChat :: [RoomChatMessage]
         , _roomName :: T.Text
         , _roomPassword :: Maybe T.Text
         , _roomPlayers :: NonEmpty User
         , _roomGameState :: Maybe GameState
         }
         deriving (Eq, Show)

newtype UserID = UserID Int
type Board = [Codeword]
type Password = T.Text

deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''UserID
deriveJSON defaultOptions ''Room
deriveJSON defaultOptions ''RoomChatMessage
deriveJSON defaultOptions ''Player
deriveJSON defaultOptions ''PlayerRole
deriveJSON defaultOptions ''Team
deriveJSON defaultOptions ''GameState
deriveJSON defaultOptions ''Codeword
deriveJSON defaultOptions ''Ownership
deriveJSON defaultOptions ''TurnPhase
deriveJSON defaultOptions ''Clue
deriveJSON defaultOptions ''CardsLeft

makeLenses ''Codeword
makeLenses ''GameState
makeLenses ''Player
makeLenses ''PlayerRole
makeLenses ''Room
makeLenses ''User

findCurrentRole :: Team -> (Team -> Player -> Bool)
  -> [Player] -> Maybe Player
findCurrentRole _ _ [] = Nothing
findCurrentRole currentTeam f (p:[])
  | f currentTeam p = Just p
  | otherwise = Nothing
findCurrentRole currentTeam f (p:ps)
  | f currentTeam p = Just p
  | otherwise = findCurrentRole currentTeam f ps


getCurrentRole :: PlayerRole -> GameState
  -> Maybe Player
getCurrentRole Speaker gs =
  findCurrentRole (_currentTurn gs) isSpeaker $ Map.elems
    $ _players gs
  where isSpeaker c (Player Speaker playerTeam _)
          | c == playerTeam = True
          | otherwise = False
        isSpeaker _ _ = False
getCurrentRole Guesser gs =
  findCurrentRole (_currentTurn gs) isGuesser $ Map.elems
    $ _players gs
  where isGuesser c (Player Guesser playerTeam _)
          | c == playerTeam = True
          | otherwise = False
        isGuesser _ _ = False
getCurrentRole _ _ = Nothing


getPlayer :: User -> GameState
  -> Maybe Player
  {-
  (^?) :: s -> Getting (First a) s a -> Maybe a
    Perform a safe head of a Fold or Traversal or
    retrieve Just the result from a Getter or Lens.
    When using a Traversal as a partial Lens, or
    a Fold as a partial Getter this can be a
    convenient way to extract the optional value.
  -}
getPlayer (User _ k) gs = Map.lookup k (_players gs)


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
makeCodeword o w = Codeword w o False

makePlayer :: PlayerRole -> Team -> User -> Player
makePlayer r t u = Player r t u

insertPlayerstoMap :: [Player] -> Map UserID Player
insertPlayerstoMap ps = foldr fn Map.empty ps
  where fn p m = Map.insert (_userID $ _user p) p m

makeNewRoom :: User -> T.Text -> Maybe Password -> Room
makeNewRoom u n p = Room u [] n p (pure u) Nothing

randomizeList :: Show a => [a] -> IO [a]
randomizeList !board = swapRandom board (99 :: Int)
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
  let (b, r) = firstTurnCards firstTurn

      blueWords = map (makeCodeword (TeamOwned Blue)) $ take b nouns
      redWords = map (makeCodeword (TeamOwned Red)) $ take r $ drop b nouns
      killerWord = map (makeCodeword Killer) $ take 1 $ drop r $ drop b nouns
      bystaderWords = map (makeCodeword Bystander) $ drop 1 $ drop r $ drop b nouns

      allWords = blueWords ++ redWords ++ killerWord ++ bystaderWords
  allWords' <- randomizeList allWords
  return allWords'


firstTurnCards :: Team -> (Int, Int)
firstTurnCards Blue = (9, 8)
firstTurnCards Red  = (8, 9)

startNewGame :: [User] -> IO GameState
startNewGame p = do
  ps <- randomizeList p

  let blueGuesser = map (makePlayer Guesser Blue) $ take 1 ps
      redGuesser = map (makePlayer Guesser Red) $ take 1 $ drop 1 ps
      blueSpeaker = map (makePlayer Speaker Blue) $ take 1 $ drop 2 ps
      redSpeaker = map (makePlayer Speaker Red) $ take 1 $ drop 3 ps
      blueListeners = map (makePlayer Listener Blue)
        $ take (listenersPerTeam) $ drop 4 ps
      redListeners = map (makePlayer Listener Red)
        $ takeWhile (const True) $ drop listenersPerTeam $ drop 4 ps

      allPlayers = blueGuesser ++ redGuesser
        ++ blueSpeaker ++ redSpeaker
        ++ blueListeners ++ redListeners

  firstTurn <- randomIO
  gb <- assignOwnership testWords firstTurn

  players' <- randomizeList allPlayers
  let playersMap = insertPlayerstoMap players'
      (cardsB, cardsR) = firstTurnCards firstTurn
  return (GameState (CardsLeft cardsB cardsR) firstTurn
    gb playersMap CluePicking False)
  where listenersPerTeam = (div (length p) 2) - 2

revealCodeword :: Codeword -> GameState
  -> GameState
revealCodeword c@Codeword{..} gs@GameState{..} =
  gs { _gameBoard = reveal (_gameBoard)
     , _turnPhase = subtractGs _turnPhase
     , _cardsLeft = subtractCard _owner _cardsLeft
     }
  where reveal (gb:gbs)
          | gb == c = c {_revealed = True} : reveal gbs
          | otherwise = gb : reveal gbs
        reveal [] = []

        subtractGs (Guessing clue@(Clue _ _ guesses)) =
          Guessing (clue {_guessesLeft = guesses - 1})
        subtractGs tP = tP

        subtractCard (TeamOwned Blue) cL =
          cL {_blueCards = (_blueCards cL) - 1}
        subtractCard (TeamOwned Red) cL =
          cL {_redCards = (_redCards cL) - 1}
        subtractCard _ cL = cL

newTurn :: GameState -> GameState
newTurn gs@GameState{..} =
  gs { _turnPhase = CluePicking
     , _currentTurn = switchTeam _currentTurn
     }

   where switchTeam Blue = Red
         switchTeam Red = Blue

makeNewTurn :: Codeword -> GameState -> TurnPhase
  -> GameState
makeNewTurn (Codeword _ (TeamOwned t) _) gs (Guessing (Clue _ _ g))
 | t /= (_currentTurn gs) = newTurn gs
 | g == 1 = newTurn gs
 | otherwise = gs
makeNewTurn (Codeword _ _ _) gs _ = newTurn gs

checkWinner :: Codeword -> GameState
  -> GameState
checkWinner c gs@GameState{..} =
  checkCards _currentTurn _cardsLeft
  where checkCards Blue (CardsLeft 0 _) = gs {_winner = True}
        checkCards Red (CardsLeft _ 0) = gs {_winner = True}
        checkCards _ _ = makeNewTurn c gs (_turnPhase)
