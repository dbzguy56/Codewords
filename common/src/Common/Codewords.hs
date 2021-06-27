{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Codewords where

import Control.Lens
import Data.Aeson.TH
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import qualified Data.Text as T
import Data.List.Index (setAt)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import qualified Data.Set as S

import System.Random

testWords :: [T.Text]
testWords = ["Apple", "Blue", "Code", "Dart", "Ear", "Fart", "Green"
            , "Horn", "Italy", "Joker", "Kart", "Lift", "Maroon"
            , "Night", "Octopus", "Popcorn", "Queen", "Realtor"
            , "Sweater", "Tree", "Uranium", "Vent", "Water"
            , "Yeast", "Zoo"]

newtype UserID = UserID Int
newtype NonEmptyText =
  NonEmptyText { getNonEmptyText :: T.Text
               }
  deriving (Eq, Show)

mkNonEmptyText :: T.Text -> Maybe NonEmptyText
mkNonEmptyText t
  | (T.strip t) == "" = Nothing
  | otherwise = Just (NonEmptyText t)
type Board = [Codeword]

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
              , _guessers :: TeamGuessers
              , _listeners :: TeamListeners
              , _speakers :: TeamSpeakers
              , _turnPhase :: TurnPhase
              , _winner :: Maybe Team
              }
  deriving (Eq, Show)

data User
  = User { _name :: NonEmptyText
         , _userID :: UserID
         }
  deriving (Eq, Show)

data TeamListeners
  = Listeners { _blueListeners :: Set UserID
              , _redListeners :: Set UserID
              }
  deriving (Eq, Show)

data TeamGuessers
  = Guessers { _blueGuesser :: UserID
             , _redGuesser :: UserID
             }
  deriving (Eq, Show)

data TeamSpeakers
  = Speakers { _blueSpeaker :: UserID
             , _redSpeaker :: UserID
             }
  deriving (Eq, Show)

data TurnPhase
  = CluePicking
  | Guessing Clue
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
  = Clue { _hint :: NonEmptyText
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

-- TODO: Tailor Gamestate to player's role
data ClientRoom
  = ClientRoom { _clientRAdminID :: UserID
               , _clientRChat :: [RoomChatMessage]
               , _clientRGameState :: Maybe GameState
               , _clientRName :: NonEmptyText
               , _clientRPassReq :: Bool
               , _clientRPlayers :: NonEmpty User
               }
               deriving (Eq, Show)

data Room
  = Room { _roomAdminID :: UserID
         , _roomChat :: [RoomChatMessage]
         , _roomName :: NonEmptyText
         , _roomPassword :: Maybe NonEmptyText
         , _roomPlayers :: NonEmpty User
         , _roomGameState :: Maybe GameState
         }
         deriving (Eq, Show)


deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''UserID
deriveJSON defaultOptions ''Room
deriveJSON defaultOptions ''RoomChatMessage
deriveJSON defaultOptions ''Team
deriveJSON defaultOptions ''TeamGuessers
deriveJSON defaultOptions ''TeamListeners
deriveJSON defaultOptions ''TeamSpeakers
deriveJSON defaultOptions ''GameState
deriveJSON defaultOptions ''Codeword
deriveJSON defaultOptions ''Ownership
deriveJSON defaultOptions ''TurnPhase
deriveJSON defaultOptions ''Clue
deriveJSON defaultOptions ''CardsLeft
deriveJSON defaultOptions ''ClientRoom
deriveJSON defaultOptions ''NonEmptyText

makeLenses ''Codeword
makeLenses ''GameState
makeLenses ''Room
makeLenses ''User
makeLenses ''TeamGuessers
makeLenses ''TeamListeners
makeLenses ''TeamSpeakers
makeLenses ''ClientRoom

tryStartGame :: User -> GameState -> Room -> Room
tryStartGame u gs r@Room{..}
  | isAdmin = r {_roomGameState = Just gs}
  | otherwise = r
  where isAdmin = _userID u == _roomAdminID


swapAtIndices :: Int -> Int -> [a] -> [a]
swapAtIndices indexX indexY list = setAt indexY x $ setAt indexX y list
  where x = list !! indexX
        y = list !! indexY

makeCodeword :: Ownership -> T.Text -> Codeword
makeCodeword o w = Codeword w o False

makeNewRoom :: User -> NonEmptyText -> Maybe NonEmptyText -> Room
makeNewRoom u n p = Room (_userID u) [] n p (pure u) Nothing

randomizeList :: Show a => [a] -> IO [a]
randomizeList board = swapRandom board (99 :: Int)
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

  let blueG = head $ map _userID $ take 1 ps
      redG = head $ map _userID $ take 1 $ drop 1 ps
      blueS = head $ map _userID $ take 1 $ drop 2 ps
      redS = head $ map _userID $ take 1 $ drop 3 ps
      blueLs = map _userID
        $ take (listenersPerTeam) $ drop 4 ps
      redLs = map _userID
        $ takeWhile (const True) $ drop listenersPerTeam $ drop 4 ps

  firstTurn <- randomIO
  gb <- assignOwnership testWords firstTurn

  let (cardsB, cardsR) = firstTurnCards firstTurn

  return (GameState (CardsLeft cardsB cardsR) firstTurn gb
    (Guessers blueG redG)
    (Listeners (S.fromList blueLs) (S.fromList redLs))
    (Speakers blueS redS)
    CluePicking Nothing)

  where listenersPerTeam = (div (length p) 2) - 2

switchTeam :: Team -> Team
switchTeam Blue = Red
switchTeam Red = Blue

revealCodeword :: Codeword -> GameState -> StdGen
  -> (GameState, StdGen)
revealCodeword c@Codeword{..} gs gen =
  let gs' = gs & gameBoard .~ (reveal $ _gameBoard gs)
               & turnPhase .~ (subtractGs $ _turnPhase gs)
               & cardsLeft .~ (subtractCard (_owner) (_cardsLeft gs))
      newGS = gs' & winner .~ (checkWinner c gs')
  in
  case (_winner newGS) of
    Just _ -> (newGS, gen)
    Nothing -> makeNewTurn _owner newGS (_turnPhase newGS) gen
  where reveal (gb:gbs)
          | gb == c = c {_revealed = True} : reveal gbs
          | otherwise = gb : reveal gbs
        reveal [] = []

        subtractGs (Guessing clue@(Clue _ _ guesses)) =
          Guessing (clue {_guessesLeft = guesses - 1})
        subtractGs CluePicking = CluePicking

        subtractCard (TeamOwned Blue) cL =
          cL {_blueCards = (_blueCards cL) - 1}
        subtractCard (TeamOwned Red) cL =
          cL {_redCards = (_redCards cL) - 1}
        subtractCard _ cL = cL


newTurn :: GameState -> StdGen -> (GameState, StdGen)
newTurn gs@GameState{..} pGen = do
  let newTeam = switchTeam _currentTurn
      (newGS, newGen) = case newTeam of
        Blue ->
          let (newG, newLs, nGen) = switchRoleUser
                (_blueGuesser $ _guessers)
                (_blueListeners $ _listeners) pGen
          in
          (gs & guessers.blueGuesser.~ newG
             & listeners.blueListeners.~ newLs
           , nGen)
        Red ->
          let (newG, newLs, nGen) = switchRoleUser
                (_redGuesser $ _guessers)
                (_redListeners $ _listeners) pGen
          in
          (gs & guessers.redGuesser.~ newG
             & listeners.redListeners.~ newLs
           , nGen)

      gs' =
        newGS { _turnPhase = CluePicking
              , _currentTurn = newTeam
              }

  (gs', newGen)

switchRoleUser :: UserID -> Set UserID -> StdGen
  -> (UserID, Set UserID, StdGen)
switchRoleUser oldUID ls gen =
  let (rNum, newGen) = randomR (0, (S.size ls) - 1) gen
      nextUID = ls ^? to S.toList . ix (rNum)
  in
  case nextUID of
    Just p -> (p, S.insert oldUID $ S.delete p ls, newGen)
    Nothing -> (oldUID, ls, gen)

makeNewTurn :: Ownership -> GameState -> TurnPhase -> StdGen
  -> (GameState, StdGen)
makeNewTurn (TeamOwned t) gs (Guessing (Clue _ _ g)) pGen
 | t /= (_currentTurn gs) = newTurn gs pGen
 | g == 0 = newTurn gs pGen
 | otherwise = (gs, pGen)
makeNewTurn _ gs _ pGen = newTurn gs pGen

checkWinner :: Codeword -> GameState
  -> Maybe Team
checkWinner (Codeword _ Killer _) gs =
  Just $ switchTeam $ _currentTurn gs
checkWinner _ gs =
  checkCards $ _cardsLeft gs
  where checkCards (CardsLeft 0 _) = Just Blue
        checkCards (CardsLeft _ 0) = Just Red
        checkCards _ = Nothing

data RoleStatus
  = NotAnyRole
  | SpeakerFor Team
  | GuesserFor Team
  | ListenerFor Team

getRoleStatus :: GameState -> UserID -> RoleStatus
getRoleStatus gs uID
  | uID == (_blueSpeaker $ _speakers gs) = SpeakerFor Blue
  | uID == (_redSpeaker $ _speakers gs) = SpeakerFor Red
  | uID == (_blueGuesser $ _guessers gs) = GuesserFor Blue
  | uID == (_redGuesser $ _guessers gs) = GuesserFor Red
  | elem uID $ _blueListeners $ _listeners gs = ListenerFor Blue
  | elem uID $ _redListeners $ _listeners gs = ListenerFor Red
  | otherwise = NotAnyRole

makeNewRole :: GameState -> UserID -> RoleStatus
  -> StdGen -> (GameState, StdGen)
makeNewRole
  gameState@(GameState cL cT gB gs@(Guessers bG rG) (Listeners bLs rLs) ss@(Speakers bS rS) tP w)
  uID uRole gen =
    case uRole of
      GuesserFor t -> do
        let (newGs, newLs, newGen) =
              case t of
                Blue -> f (switchRoleUser uID bLs gen) t
                Red -> f (switchRoleUser uID rLs gen) t
        (GameState cL cT gB newGs newLs ss tP w, newGen)

      SpeakerFor t -> do
        let (newSs, newLs, newGen) =
              case t of
                Blue -> f' (switchRoleUser uID bLs gen) t
                Red -> f' (switchRoleUser uID rLs gen) t
        (GameState cL cT gB gs newLs newSs tP w, newGen)

      ListenerFor t -> do
        let newLs =
              case t of
                Blue -> Listeners (S.filter ((/=) uID) bLs) rLs
                Red -> Listeners bLs (S.filter ((/=) uID) rLs)
        (GameState cL cT gB gs newLs ss tP w, gen)
      NotAnyRole -> (gameState, gen)

  where f (newBG, blueLs, stdG) Blue = ((Guessers newBG rG)
          , Listeners (S.filter ((/=) uID) blueLs) rLs, stdG)
        f (newRG, redLs, stdG) Red = ((Guessers bG newRG)
          , Listeners bLs (S.filter ((/=) uID) redLs), stdG)

        f' (newBS, blueLs, stdG) Blue = ((Speakers newBS rS)
          , Listeners (S.filter ((/=) uID) blueLs) rLs, stdG)
        f' (newRS, redLs, stdG) Red = ((Speakers bS newRS)
          , Listeners bLs (S.filter ((/=) uID) redLs), stdG)
