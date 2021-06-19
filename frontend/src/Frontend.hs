{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Frontend where

import Common.Codewords
import Common.Protocol
import Common.Route
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class

import Data.Bool (bool)
import Data.Char (isAlpha)
import Data.IntMap (IntMap, insert, delete)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.IntMap as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Reflex.Class as R

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Language.Javascript.JSaddle
{-
import GHCJS.DOM (currentWindowUnchecked)
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Location as Location
-}
import Reflex.Dom.Core

type CodewordsM t m = ( DomBuilder t m , PostBuild t m , TriggerEvent t m
                      , HasJSContext (Performable m)
                      , MonadJSM m, MonadHold t m, MonadFix m
                      , DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m)
                      , MonadIO (Performable m), PerformEvent t m
                      , MonadSample t (Performable m)
                      , HasWebView m
                      )


data View
  = SignIn
  | LoggedIn User LoggedInView
  deriving Eq

data LoggedInView
  = HomeView
  | RoomView Int RoomStateView
  deriving Eq

data RoomStateView
  = LobbyView
  | GameView
  deriving Eq

data InvalidHintInput
  = FieldEmpty
  | MultipleWords
  | InvalidChars
  | ContainsCodeword
  deriving Show

codeWordsSocket :: CodewordsM t m
  => Event t ClientMsg -> m (Event t ServerMsg)
codeWordsSocket send = do
  rawWebsocket <- jsonWebSocket "wss://codewords.app/websocket" $ def
  --rawWebsocket <- jsonWebSocket "ws://localhost:8000/websocket" $ def
    & webSocketConfig_send .~ (fmap pure send)

  return $ fmapMaybe id $ _webSocket_recv rawWebsocket
{-
codeWordsSocket :: CodewordsM t m
  => Event t ClientMsg -> m (Event t ServerMsg)
codeWordsSocket send = do
  {-
    pure :: a -> f a
      Lift a value.

    return :: a -> m a
      Inject a value into the monadic type.
  -}
  wsPath <- getWebsocketPath
  rawWebsocket <- jsonWebSocket (wsPath <> "websocket") $ def
    & webSocketConfig_send .~ (fmap pure send)

  return $ fmapMaybe id $ _webSocket_recv rawWebsocket

getWebsocketPath :: MonadJSM m => m (T.Text)
getWebsocketPath = liftJSM $ do
  loc <- currentWindowUnchecked >>= Window.getLocation
  host <- Location.getHost loc
  proto <- Location.getProtocol loc
  return $ buildUrl host proto
  where
    buildUrl :: T.Text -> T.Text -> T.Text
    buildUrl h p =
      let
        wsProtocol = case p of
          "http:" -> "ws:"
          "https:" -> "wss:"
          a -> a
      in
      wsProtocol <> "/" <> h <> "/"
-}
showMsgs :: (MonadHold t m, MonadFix m
  , DomBuilder t m, PostBuild t m)
  => Event t T.Text -> m ()
showMsgs msg = do

  msgs <- foldDyn (:) [] msg
  simpleList msgs $ el "div" . dynText
  return ()

roomChatWidget :: CodewordsM t m => Int -> User -> Dynamic t (Maybe GameState)
  -> Dynamic t [RoomChatMessage] -> m (Event t ClientMsg)
roomChatWidget n u dmGS dRC = do
  elClass "div" "bg-gray-300 flex flex-col-reverse h-1/3 lg:h-full overflow-auto" $
    elClass "div" "flex flex-col-reverse break-all" $
      simpleList dRC (\rChat -> do
        el "div" $ do
          let uS = fmap userSpeaking rChat
              tColor = zipDynWith (maybe "text-purple-600") (getTColor <$> uS) dmGS

          elDynClass "span" tColor $ dynText $ fmap _name uS
          text ": "
          dynText $ fmap chatMessage rChat
        )

  dGS <- maybeDyn dmGS
  msgStream <- dyn $ ffor dGS $ \case
    Just gs -> do
      e2 <- dyn $ ffor ((flip getRoleStatus $ _userID u) <$> gs) $ \case
        SpeakerFor _ -> do
          w <- holdUniqDyn $ _winner <$> gs
          dyn (speakerInput <$> w) >>= switchHold never
        _ -> showInput
      switchHold never e2
    Nothing -> showInput

  e' <- switchHold never msgStream
  return $ fmap (SendRoomChatMsg n) e'

  where showInput = elClass "div" "flex flex-row" $ do
          eventText <- inputTextBoxBtnWidget
            "Send" "width:85%"
          return $ fmap (RoomChatMessage u) eventText

        speakerInput (Just _) = showInput
        speakerInput _ = return never

        getTColor uS x = teamColor $
          maybe Blue snd $ getUserRoleTeam (_userID uS) x


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


styleCard :: RoleStatus -> Codeword -> GameState -> T.Text
styleCard _ (Codeword _ o True) _ = T.pack $ (++) (cardColor o) "700"
styleCard (SpeakerFor _) (Codeword _ o _) _ = T.pack $ (++) (cardColor o) "300"
styleCard _ _ _ = T.pack $ "bg-gray-400"

cardColor :: Ownership -> [Char]
cardColor Bystander = "bg-green-"
cardColor Killer = "bg-gray-"
cardColor (TeamOwned Red) = "bg-red-"
cardColor (TeamOwned Blue) = "bg-blue-"

displayRoleBar :: CodewordsM t m => User
  -> Dynamic t GameState -> m ()
displayRoleBar u dGS = do
  elClass "div" "flex justify-between" $ do
    elClass "div" "" $ do
      text "Current Turn: "
      elDynClass "span" (teamColor <$> (_currentTurn <$> dGS)) $
        dynText $ (tShow <$> (_currentTurn <$> dGS))

    mURoleTeam <- maybeDyn $ (getUserRoleTeam (_userID u)) <$> dGS
    dyn_ $ ffor mURoleTeam $ \case
      Just dRT -> do
        elClass "div" "" $ do
          text "Your Role: "
          elDynClass "span" (teamColor.snd <$> dRT) $
            dynText $ tShow.snd <$> dRT
          el "span" $ dynText $ T.append (T.pack "'s ") <$> fst <$> dRT

      Nothing -> do
        el "span" $ text "Couldnt find player"


getCurrentGuesserName :: GameState -> NonEmpty User -> Maybe T.Text
getCurrentGuesserName gs us = getName (NE.toList us) $ getUID (_currentTurn gs)
  where getUID Blue = _blueGuesser $ _guessers gs
        getUID Red = _redGuesser $ _guessers gs

getCurrentSpeakerName :: GameState -> NonEmpty User -> Maybe T.Text
getCurrentSpeakerName gs us = getName (NE.toList us) $ getUID (_currentTurn gs)
  where getUID Blue = _blueSpeaker $ _speakers gs
        getUID Red = _redSpeaker $ _speakers gs

getName :: [User] -> UserID -> Maybe T.Text
getName (p:ps) uID
  | uID == (_userID p) = Just (_name p)
  | otherwise = getName ps uID
getName [] _ = Nothing


getUserRoleTeam :: UserID -> GameState -> Maybe (T.Text, Team)
getUserRoleTeam uID gs = do
  role $ getRoleStatus gs uID
  where role (SpeakerFor t) = Just ("Speaker", t)
        role (GuesserFor t) = Just ("Guesser", t)
        role (ListenerFor t) = Just ("Listener", t)
        role _ = Nothing

justTrue :: Maybe a -> Bool
justTrue (Just _) = True
justTrue _ = False

gameBoardUI :: CodewordsM t m => UserID
  -> Dynamic t GameState -> m (Event t Codeword)
gameBoardUI uID gs = elClass "div" "grid grid-cols-5 gap-4 p-4 h-full" $ do
  eList <- simpleList (_gameBoard <$> gs) (codewordUI uID gs)
  return $ switchDyn $ leftmost <$> eList

codewordUI :: CodewordsM t m => UserID -> Dynamic t GameState
  -> Dynamic t Codeword -> m (Event t Codeword)
codewordUI uID gs c = do
  (e, _) <- elDynClass' "div" (mkStyle <$> c <*> gs) $ dynText
    $ _word <$> c

  let roleStatus = (flip getRoleStatus uID) <$> gs
      cTeam = _currentTurn <$> gs
      bCGuesser = isCurrentGuesser <$> roleStatus <*> cTeam
      clickE = gate (current bCGuesser) $ tag (current c) $ domEvent Click e

  return clickE
  where mkStyle c' gs' =
          case (_winner gs') of
            Just _ -> "p-4 flex justify-center rounded-lg items-center "
              <> (styleCard (SpeakerFor Blue) c' gs')
            Nothing -> "p-4 flex justify-center rounded-lg items-center "
              <> (styleCard (getRoleStatus gs' uID) c' gs')

displayBottomBar :: CodewordsM t m => UserID -> Dynamic t Bool
  -> Int -> Dynamic t GameState -> m (Event t ClientMsg)
displayBottomBar uID adminStatus roomID dGS = do
  elClass "div" "flex" $ do
    backBtn <- elClass "span" "" $
      btnWidget "bg-gray-600" "Leave"

    dGS' <- holdUniqDyn dGS
    elClass "span" "flex flex-col text-base" $ do
      elClass "span" "text-center" $ text "CARDS LEFT"
      elClass "span" "" $ do
        elClass "span" "text-blue-600" $ text "BLUE: "
        elClass "span" "" $ dynText $ tShow._blueCards._cardsLeft <$> dGS'

        elClass "span" "text-red-600" $ text "RED: "
        elClass "span" "" $ dynText $ tShow._redCards._cardsLeft <$> dGS'

    endTurnBtn <- ifGuessing (isCurrentGuesser <$>
      ((flip getRoleStatus uID) <$> dGS') <*> (_currentTurn <$> dGS'))
      roomID (_turnPhase <$> dGS')

    let endBtnStyle b = bool "opacity-0 pointer-events-none"
          "opacity-100 pointer-events-auto" b

    endGameBtn <- elDynClass "span" (endBtnStyle <$> adminStatus) $
      btnWidget "bg-gray-600" "END GAME"

    return $ leftmost [endTurnBtn, EndGame roomID <$ endGameBtn
      , LeaveRoom roomID <$ backBtn]

ifGuessing :: CodewordsM t m => Dynamic t Bool -> Int -> Dynamic t TurnPhase
  -> m (Event t ClientMsg)
ifGuessing dCurrentG roomID dTP = do
  dTP' <- holdUniqDyn dTP
  e' <- dyn $ ffor dTP' $ \case
    (Guessing (Clue _ _ g)) -> do
      elClass "span" "" $ text $ "GUESSES LEFT: "
        <> (tShow g)
      e <- dyn $ ffor dCurrentG $ \case
        True -> do
          btn <- elClass "span" "" $ btnWidget "bg-gray-600" "END TURN"
          return $ EndTurn roomID <$ btn
        False -> return never
      switchHold never e
    _ -> return never
  switchHold never e'

gameBoardWidget :: CodewordsM t m => Int -> Dynamic t Bool
  -> Dynamic t Room -> User -> m (Event t ClientMsg)
gameBoardWidget n adminStatus r u = do
  dmGS <- maybeDyn $ _roomGameState <$> r
  e <- dyn $ ffor dmGS $ \case
    Just gs -> do
      displayRoleBar u gs
      c <- gameBoardUI (_userID u) gs
      bottomE <- displayBottomBar (_userID u) adminStatus n gs

      return $ leftmost [ChangeGameState n <$> c,
        bottomE]
    Nothing -> return never
  switchHold never e

btnWidget :: CodewordsM t m => T.Text -> T.Text -> m (Event t ())
btnWidget style t = do
  (e, _)  <- elClass' "button" style $ text t
  return $ domEvent Click e

isCurrentSpeaker :: RoleStatus -> Team -> Bool
isCurrentSpeaker (SpeakerFor userT) currentT = userT == currentT
isCurrentSpeaker _ _ = False

isCurrentGuesser :: RoleStatus -> Team -> Bool
isCurrentGuesser (GuesserFor userT) currentT = userT == currentT
isCurrentGuesser _ _ = False

displaySideBar :: CodewordsM t m => Int -> User
  -> Dynamic t Room -> m (Event t ClientMsg)
displaySideBar n u r = do
  rGS <- maybeDyn (_roomGameState <$> r)
  e <- dyn $ ffor rGS $ \case
    Just gs -> do
      let dCSpeaker = isCurrentSpeaker <$> ((flip getRoleStatus $ _userID u)
            <$> gs) <*> (_currentTurn <$> gs)
          dSpeakerCluePicking = (&&) <$> dCSpeaker
            <*> (isCluePicking <$> (_turnPhase <$> gs))
      dSCP <- holdUniqDyn dSpeakerCluePicking
      e' <- dyn $ ffor dSCP $ \case
        True -> displaySpeakerView n (_gameBoard <$> gs)
        False -> roomChatWidget n u (_roomGameState <$> r) (fmap (_roomChat) r)
      switchHold never e'
    Nothing -> roomChatWidget n u (_roomGameState <$> r) (fmap (_roomChat) r)
  switchHold never e
  where isCluePicking CluePicking = True
        isCluePicking _ = False


parseHintInput :: Board -> T.Text -> Int
  -> Either InvalidHintInput Clue
parseHintInput b t g
  | (T.strip t) == "" = Left FieldEmpty
  | (length $ T.words t) > 1 = Left MultipleWords
  | checkNonLetters (T.toLower $ T.strip t) = Left InvalidChars
  | checkCodewords (T.toLower $ T.strip t) b = Left ContainsCodeword
  | otherwise = Right (Clue (T.strip t) g (g + 1))

  where checkNonLetters w = not $ T.null
          $ T.dropWhile isAlpha w

        checkCodewords _ [] = False
        checkCodewords w (c:[]) = w == (T.toLower $ _word c)
        checkCodewords w (c:cs)
          | w == (T.toLower $ _word c) = True
          | otherwise = checkCodewords w cs


hintErrMsg :: InvalidHintInput -> T.Text
hintErrMsg FieldEmpty = "Hint cannot be empty!"
hintErrMsg MultipleWords = "Hint can only be one word!"
hintErrMsg InvalidChars = "Hint cannot contain invalid characters!"
hintErrMsg ContainsCodeword = "Hint cannot contain a codeword!"


displaySpeakerView :: CodewordsM t m => Int
  -> Dynamic t Board -> m (Event t ClientMsg)
displaySpeakerView n dGB = do
  cMsg <- elClass "div" "flex flex-col justify-between bg-gray-600 \
    \ h-full text-center" $ mdo

    elClass "div" "" $ text "Give your team a one word hint!"

    eitherC <- elClass "div" "" $ do
      elClass "div" "" $ text "HINT"
      h <- inputElement $ def

      let elVal = _inputElement_value h
          eitherClue = parseHintInput <$> dGB <*> elVal <*> guesses
      eitherClue' <- eitherDyn eitherClue
      dyn_ $ ffor eitherClue' $ \case
        Left a -> do
          elClass "div" "bg-red-300" $ dynText $ hintErrMsg <$> a
        _ -> return ()

      return eitherClue

    guesses <- elClass "div" "" $ do
      elClass "div" "" $ text "RELATED WORDS TO HINT"
      g <- elClass "div" "flex" $ mdo
        dInt' <- elClass "div" "bg-gray-50 rounded-md" $ do
          dInt <- foldDyn ($) (1 :: Int) bClicked
          display dInt
          return dInt

        bClicked <- elClass "div" "" $ do
          increment <- btn "▲" incFn
          decrement <- btn "▼" decFn

          return $ leftmost [
              increment
            , decrement]

        return dInt'
      return g


    let btnColor = "bg-gray-400"
    (e, _) <- elClass' "button" btnColor (text "SUBMIT HINT")

    eitherC' <- eitherDyn eitherC
    e2 <- dyn $ ffor eitherC' $ \case
      Right c ->
        return $ tag (current $ SendClue n <$> c) $ domEvent Click e
      Left _ -> return never
    switchHold never e2

  return cMsg
  where btn label fn = do
          (e, _) <- el' "button" $ text label
          return $ fn <$ domEvent Click e

        incFn 8 = 8
        incFn x = x + 1
        decFn 0 = 0
        decFn x = subtract 1 x


displayTopMessageBar :: CodewordsM t m => Dynamic t Room
  -> m ()
displayTopMessageBar r = do
  elDynClass "div" "flex whitespace-pre-wrap \
    \ justify-center bg-gray-600 h-auto lg:text-3xl" $ do
    rGs <- maybeDyn (_roomGameState <$> r)
    dyn_ $ ffor rGs $ \case
      Just gs -> do
        dWinner <- maybeDyn (_winner <$> gs)
        dyn_ $ ffor dWinner $ \case
          Just t -> do
            elDynClass "span" (teamColor <$> t) $
              dynText $ T.pack.show <$> t
            text " team wins!"
          Nothing -> do
            let rPs = _roomPlayers <$> r
            turnPhaseMsg gs rPs

      Nothing -> do
        let rPs = (NE.toList._roomPlayers) <$> r
            adminID = _roomAdminID <$> r

        dAdminName <- maybeDyn (getName <$> rPs <*> adminID)
        dyn_ $ ffor dAdminName $ \case
          Just p -> do
            text "Waiting on "
            elClass "span" "text-yellow-300" $ dynText p
            text " to start the game"
          Nothing ->
            text "Could not get admin name"
    return ()
  return ()

turnPhaseMsg :: CodewordsM t m => Dynamic t GameState
  -> Dynamic t (NonEmpty User) -> m ()
turnPhaseMsg dGS dPs = do
  dyn_ $ ffor (_turnPhase <$> dGS) $ \case
    CluePicking -> do
      let dCSpeaker = getCurrentSpeakerName <$> dGS <*> dPs
      dyn_ $ ffor dCSpeaker $ \case
        Just p -> do
          elDynClass "span" (teamColor <$> (_currentTurn <$> dGS))
            $ text p
          text  " is thinking of a hint."
          return ()
        Nothing ->
          text "Cannot find Speaker"

    (Guessing (Clue h rWords _)) -> do
      elClass "span" "flex justify-between w-full px-2" $ do
        el "div" $ do
          elClass "span" "color-red-600" $ text $ "HINT: " <> h

        el "div" $ do
          text "Guesser: "
          let dCGuesser = getCurrentGuesserName <$> dGS <*> dPs
          dyn_ $ ffor dCGuesser $ \case
            Just p ->
              elDynClass "span" (teamColor <$> (_currentTurn <$> dGS)) $ text p
            Nothing ->
              el "div" $ text "Cannot find Guesser"

        el "div" $ text $ "RELATED WORDS: " <> tShow rWords

teamColor :: Team -> T.Text
teamColor Blue = "text-blue-600"
teamColor Red = "text-red-600"

openRoomInfoDialog :: CodewordsM t m => Event t Bool
  -> T.Text -> m (Event t T.Text)
openRoomInfoDialog eToggle initialRoomName = mdo
  dOpen <- holdDyn False $ leftmost [eToggle, False <$ eClose]
  let mkStyles b = T.intercalate " "
        ["absolute flex justify-center \
          \ backdrop-filter backdrop-blur-sm items-center inset-0"
        , bool "opacity-0 pointer-events-none"
          "opacity-100 pointer-events-auto" b]

  eClose <- elDynClass "div" (mkStyles <$> dOpen) $ do
    elClass "div" "bg-red-500 w-1/2 h-1/2" $ do
      iE <- inputElement $ def
        & inputElementConfig_elementConfig
          . elementConfig_initialAttributes
          .~ ("autofocus" =: ""
            <> "placeholder" =: "Type something here...")
        & inputElementConfig_initialValue .~ initialRoomName

      let btnStyles = "ml-2 px-1 bg-gray-600 rounded-lg"
      (cancelBtn, _) <- elClass' "span" "" $
        btnWidget btnStyles "Cancel"
      (saveBtn, _) <- elClass' "span" "" $
        btnWidget btnStyles "Save"

      let saveE = tag (current $ _inputElement_value iE) $ domEvent Click saveBtn
          cancelE = domEvent Click cancelBtn

      return $ leftmost [saveE, initialRoomName <$ cancelE]

  return $ ffilter (\x -> ((/=) initialRoomName x) && ((/=) "" x)) eClose


handleChangeRoomInfo :: CodewordsM t m => Int -> Dynamic t Room
  -> m (Event t ClientMsg)
handleChangeRoomInfo n dR = do
  btn <- elClass "span" "" $
    btnWidget "ml-2 px-1 bg-gray-600 rounded-lg" "Change"

  eT <- dyn $ (openRoomInfoDialog (True <$ btn)) <$> (_roomName <$> dR)
  eT' <- switchHold never eT

  return $ ChangeRoomName n <$> eT'


displayRoom :: CodewordsM t m => Int -> Dynamic t Room -> User
  -> RoomStateView -> m (Event t ClientMsg)
displayRoom n r u v = do
  r' <- holdUniqDyn r
  elClass "div" "flex flex-col h-screen select-none" $ do
    let rAdminID = fmap _roomAdminID r'
    let adminStatus = isUserAdmin rAdminID (return u)
    elClass "div" "flex justify-center bg-gray-700 xl:h-16 \
      \ 2xl:h-8 rounded lg:text-5xl" $
      text "GET UR BEEG YOSHI HERE"

    displayTopMessageBar r'

    elClass "div" "flex flex-col text-5xl md:flex-row h-full overflow-hidden lg:text-3xl" $ do
      btn <- elClass "div" "flex flex-col flex-grow bg-gray-500 rounded h-full" $ do
        case v of
          LobbyView -> do
            eRN' <- elClass "div" "flex" $ do
              eRN <- elDynClass "div" "p-1 flex-grow bg-gray-700 rounded-t" $ do
                dynText $ fmap (\room -> "Room: " <> _roomName room) r'
                displayIfAdmin adminStatus (handleChangeRoomInfo n r') (return never)

              elClass "div" "lg:px-10 2xl:px-3 bg-gray-600 rounded-full" $ text "?"
              return eRN

            displayRoomUsers rAdminID (fmap _roomPlayers r')

            eBtn <- elClass "div" "flex flex-row justify-between h-1/6" $ do
              let btnStyles = "m-2 p-1 bg-gray-600 rounded-lg"
              backBtn <- btnWidget btnStyles "Leave"

              startBtn <- displayIfAdmin adminStatus
                (btnWidget btnStyles "Start") (return never)
              startBtn' <- switchHold never startBtn

              return $ leftmost [ LeaveRoom n <$ backBtn
                , StartGame n <$ startBtn']

            eRoomName <- switchHold never eRN'
            return $ leftmost [eBtn, eRoomName]

          GameView -> do
            clientMsg <- gameBoardWidget n adminStatus r' u
            return clientMsg

      cMsg <- elClass "div" "flex flex-col h-full lg:w-1/4" $ do
        displaySideBar n u r'

      return $ leftmost [cMsg, btn]

isUserAdmin :: Reflex t => Dynamic t UserID
  -> Dynamic t User -> Dynamic t Bool
isUserAdmin adminID u = (==) <$> (fmap _userID u) <*> adminID

displayIfAdmin :: CodewordsM t m => Dynamic t Bool -> m b -> m b
  -> m (Event t b)
displayIfAdmin adminStatus adminAction defAction
  = dyn $ fmap doFn adminStatus
  where doFn False = defAction
        doFn True  = adminAction


displayRoomCard :: CodewordsM t m => Dynamic t (Int, Room)
  -> m (Event t Int)
displayRoomCard r = do
  (e, _) <- el' "div" $ dynText $ fmap (_roomName.snd) r
  return $ tag (current $ fmap fst r) $ domEvent Click e


displayRoomUsers :: CodewordsM t m => Dynamic t UserID
  -> Dynamic t (NonEmpty User) -> m ()
displayRoomUsers adminID users = elClass "div" "px-4 h-full" $ do
  elClass "div" "flex justify-center py-2" $ text "Players"
  elClass "div" "grid grid-cols-2 gap-4 px-2" $
    simpleList (fmap (reverse.NE.toList) users) (\u -> do
        let adminStatus = isUserAdmin adminID u
        let defaultStyle = "px-2 bg-gray-400 rounded"
        elDynClass "div" (T.append <$> (defaultStyle) <*>
          (fmap makeAdminStyle adminStatus)) $ dynText $ fmap _name u
      )
  return ()
  where makeAdminStyle True  = " text-yellow-300"
        makeAdminStyle False = ""

roomsWidget :: CodewordsM t m => Event t (IntMap Room)
  -> Event t (Int, Room) -> Event t Int -> m (Dynamic t (IntMap Room))
roomsWidget rList e deleteEvent =
  foldDyn ($) mempty $ leftmost [ fmap mappend rList
  , fmap (\(i, r) -> insert i r) e
  , fmap delete deleteEvent]


roomListWidget :: CodewordsM t m => Dynamic t (IntMap Room)
  -> m (Event t Int)
roomListWidget rList = do
  eList <- el "div" $ simpleList (fmap M.toList rList) displayRoomCard
  return $ switchDyn $ leftmost <$> eList


inputTextBoxBtnWidget :: CodewordsM t m
  => T.Text -> T.Text -> m (Event t T.Text)
inputTextBoxBtnWidget btnText style = mdo
  inputTextBox <- inputElement $ def
    & inputElementConfig_elementConfig
    . elementConfig_initialAttributes
    .~ ("autofocus" =: "" <> "style" =: style
      <> "placeholder" =: "Type something here...")
    & inputElementConfig_setValue .~ ("" <$ send)

  (e, _) <- elClass' "button" "bg-gray-100" $ text btnText

  let clicked = domEvent Click e
      enter = keypress Enter (_inputElement_element
        inputTextBox)
      send = leftmost [clicked, enter]
      elVal = _inputElement_value inputTextBox

  e' <- dyn $ ffor ((&&) <$> (notEmpty <$> elVal)
    <*> ((/=) "SYSTEM" <$> elVal)) $ \case
    False -> return never
    True -> return $ tag (current elVal) send
  switchHold never e'
  where notEmpty t
          | (T.strip t) == "" = False
          | otherwise = True

signInWidget :: CodewordsM t m => m (Event t ClientMsg)
signInWidget = do
  elClass "div" "text-5xl" $ do
    eventText <- inputTextBoxBtnWidget "Choose this name" ""
    return $ fmap CreateName eventText

homeViewWidget :: CodewordsM t m => Dynamic t (IntMap Room) -> User
  -> m (Event t ClientMsg)
homeViewWidget rList u = do
  el "div" $ text $ "Welcome " <> _name u <> " ID: " <> tShow (_userID u)
  e <- inputTextBoxBtnWidget "Create Room" ""
  roomClicked <- roomListWidget rList
  return $ leftmost [fmap JoinRoom roomClicked, fmap (flip CreateRoom Nothing) e]

changeView :: ServerMsg -> View -> View
changeView (GameStarted rID) (LoggedIn u _) = LoggedIn u $ RoomView rID GameView
--changeView (GameStateChanged rID) (LoggedIn u _) = LoggedIn u $ RoomView rID GameView
changeView (LeftRoom u) _ = LoggedIn u HomeView
changeView (NameCreated u) _ = LoggedIn u HomeView
changeView (RoomCreated rID _) (LoggedIn u _) = LoggedIn u $ RoomView rID LobbyView
changeView (GameEnded rID) (LoggedIn u _) = LoggedIn u $ RoomView rID LobbyView
-- TODO: Allow user to join mid game possibly?
changeView (RoomJoined rID) (LoggedIn u _) = LoggedIn u $ RoomView rID LobbyView
changeView _ v = v

router :: CodewordsM t m => Dynamic t (IntMap Room) -> View
  -> m (Event t ClientMsg)
router _ SignIn = signInWidget
router r (LoggedIn u HomeView) = homeViewWidget r u
router roomList (LoggedIn u (RoomView n v)) = do
  let currentRoom = M.lookup n <$> roomList
  currentRoom' <- maybeDyn currentRoom
  clientMsg <- dyn $ ffor currentRoom' $ \case
    Just r -> displayRoom n r u v
    Nothing -> text "Room does not exist" >> return never
  switchHold never clientMsg

tShow :: Show a => a -> T.Text
tShow = T.pack.show

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Codewords"
      -- <link href="https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css" rel="stylesheet">
      elAttr "link" ("href" =: "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css"
        <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css"
        <> "rel" =: "stylesheet") blank
      -- <meta name="viewport" content="width=device-width, initial-scale=1">
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, \
        \initial-scale=1") blank
  , _frontend_body = elClass "div" "w-screen h-screen bg-gray-500" $ do

      prerender_ blank $ liftJSM $ void $ eval ("" :: T.Text)
      prerender_ blank $ mdo

        serverMsg <- codeWordsSocket newClientMsg

        dView <- foldDyn changeView SignIn serverMsg
        dView' <- holdUniqDyn dView
        clientMsg <- dyn $ router roomList <$> dView'
        newClientMsg <- switchHold never clientMsg

        roomList <- roomsWidget (fmapMaybe (preview _RoomList) serverMsg)
          (leftmost [fmapMaybe (preview _RoomChanged) serverMsg
          , fmapMaybe (preview _RoomCreated) serverMsg])
          $ fmapMaybe (preview _RoomDeleted) serverMsg

        return ()
      return ()
  }
