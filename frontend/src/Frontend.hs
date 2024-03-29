{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Applicative
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

--import qualified Debug.Trace as DBUG

import Reflex.Class as R

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Language.Javascript.JSaddle

import GHCJS.DOM (currentWindowUnchecked)
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Location as Location

import Reflex.Dom.Core

type CodewordsM t m = ( DomBuilder t m , PostBuild t m , TriggerEvent t m
                      , MonadJSM m, MonadHold t m, MonadFix m
                      , DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m)
                      , MonadIO (Performable m), PerformEvent t m
                      , MonadSample t (Performable m)
                      )


data View
  = SignIn
  | LoggedIn User LoggedInView
  deriving Eq

data LoggedInView
  = HomeView (Maybe Int)
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
  elClass "div" "bg-gray-300 flex flex-col-reverse h-full overflow-auto" $
    elClass "div" "flex flex-col-reverse break-all" $
      simpleList dRC (\rChat -> do
        el "div" $ do
          let uS = fmap userSpeaking rChat
              tColor = zipDynWith (maybe "text-purple-600") (getTColor <$> uS) dmGS

          elDynClass "span" tColor $ dynText $ getNonEmptyText <$> fmap _name uS
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
  | uID == (_userID p) = Just $ getNonEmptyText (_name p)
  | otherwise = getName ps uID
getName [] _ = Nothing


getUserRoleTeam :: UserID -> GameState -> Maybe (T.Text, Team)
getUserRoleTeam uID gs = do
  role $ getRoleStatus gs uID
  where role (SpeakerFor t) = Just ("Speaker", t)
        role (GuesserFor t) = Just ("Guesser", t)
        role (ListenerFor t) = Just ("Listener", t)
        role _ = Nothing

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
  -> Dynamic t ClientRoom -> User -> m (Event t ClientMsg)
gameBoardWidget n adminStatus r u = do
  dmGS <- maybeDyn $ _clientRGameState <$> r
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
  -> Dynamic t ClientRoom -> m (Event t ClientMsg)
displaySideBar n u r = do
  rGS <- maybeDyn (_clientRGameState <$> r)
  e <- dyn $ ffor rGS $ \case
    Just gs -> do
      let dCSpeaker = isCurrentSpeaker <$> ((flip getRoleStatus $ _userID u)
            <$> gs) <*> (_currentTurn <$> gs)
          dSpeakerCluePicking = (&&) <$> dCSpeaker
            <*> (isCluePicking <$> (_turnPhase <$> gs))
      dSCP <- holdUniqDyn dSpeakerCluePicking
      e' <- dyn $ ffor dSCP $ \case
        True -> displaySpeakerView n (_gameBoard <$> gs)
        False -> roomChatWidget n u (_clientRGameState <$> r) (fmap (_clientRChat) r)
      switchHold never e'
    Nothing -> roomChatWidget n u (_clientRGameState <$> r) (fmap (_clientRChat) r)
  switchHold never e
  where isCluePicking CluePicking = True
        isCluePicking _ = False


parseHintInput :: Board -> NonEmptyText -> Int
  -> Either InvalidHintInput Clue
parseHintInput b (NonEmptyText t) g
  | (length $ T.words t) > 1 = Left MultipleWords
  | checkNonLetters (T.toLower $ T.strip t) = Left InvalidChars
  | checkCodewords (T.toLower $ T.strip t) b = Left ContainsCodeword
  | otherwise = Right (Clue (NonEmptyText (T.strip t)) g (g + 1))

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
  elClass "div" "flex flex-col justify-between bg-gray-600 \
    \ h-full text-center" $ mdo

    elClass "div" "" $ text "Give your team a one word hint!"
    dMDEitherClue' <- elClass "div" "" $ do
      elClass "div" "" $ text "HINT"
      h <- inputElement $ def


      let elVal = _inputElement_value h
          dMElVal = mkNonEmptyText <$> elVal
      -- Dynamic (Maybe ( Dynamic (Either InvalidHintInput Clue)))
      dMDEitherClue <- maybeDyn $ f' <$> (f <$> dGB <*> guesses) <*> dMElVal
      dyn_ $ ffor dMDEitherClue $ \case
        Just dEInvalidClue ->
          dyn_ $ ffor dEInvalidClue $ \case
            Left a -> elClass "div" "bg-red-300" $ text $ hintErrMsg a
            Right _ -> return ()
        Nothing -> return ()

      return dMDEitherClue

    guesses <- elClass "div" "" $ do
      elClass "div" "" $ text "RELATED WORDS TO HINT"
      g <- elClass "div" "flex justify-center" $ mdo
        dInt' <- elClass "div" "flex items-center text-5xl bg-gray-50 text-center" $ do
          dInt <- foldDyn ($) (1 :: Int) bClicked
          display dInt
          return dInt

        bClicked <- elClass "div" "flex flex-col" $ do
          increment <- btn "▲" incFn
          decrement <- btn "▼" decFn

          return $ leftmost [
              increment
            , decrement]

        return dInt'
      return g

    let btnColor = "bg-gray-400"
    (e, _) <- elClass' "button" btnColor (text "SUBMIT HINT")

    return $ switchDyn $ ffor dMDEitherClue' $ \case
      Just dEInvalidClue' -> do
        switchDyn $ ffor dEInvalidClue' $ \case
          Right c' -> (SendClue n c') <$ domEvent Click e
          Left _ -> never
      Nothing -> never

  where btn label fn = do
          (e, _) <- elClass' "button" "bg-gray-200" $ text label
          return $ fn <$ domEvent Click e

        incFn 8 = 8
        incFn x = x + 1
        decFn 0 = 0
        decFn x = subtract 1 x

        f :: Board -> Int
          -> (NonEmptyText -> (Either InvalidHintInput Clue))
        f gb gs = (flip (parseHintInput gb) gs)

        f' :: (NonEmptyText -> (Either InvalidHintInput Clue))
          -> Maybe NonEmptyText -> Maybe (Either InvalidHintInput Clue)
        f' partialF mNE = partialF <$> mNE


displayTopMessageBar :: CodewordsM t m => Dynamic t ClientRoom
  -> m ()
displayTopMessageBar r = do
  elDynClass "div" "flex whitespace-pre-wrap \
    \ justify-center bg-gray-600 h-auto lg:text-3xl" $ do
    rGs <- maybeDyn (_clientRGameState <$> r)
    dyn_ $ ffor rGs $ \case
      Just gs -> do
        dWinner <- maybeDyn (_winner <$> gs)
        dyn_ $ ffor dWinner $ \case
          Just t -> do
            elDynClass "span" (teamColor <$> t) $
              dynText $ T.pack.show <$> t
            text " team wins!"
          Nothing -> do
            let rPs = _clientRPlayers <$> r
            turnPhaseMsg gs rPs

      Nothing -> do
        let rPs = (NE.toList._clientRPlayers) <$> r
            adminID = _clientRAdminID <$> r

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
          elClass "span" "text-yellow-500" $ text $ "HINT: " <> (getNonEmptyText h)

        el "div" $ do
          text "Guesser: "
          let dCGuesser = getCurrentGuesserName <$> dGS <*> dPs
          dyn_ $ ffor dCGuesser $ \case
            Just p ->
              elDynClass "span" (teamColor <$> (_currentTurn <$> dGS)) $ text p
            Nothing ->
              el "div" $ text "Cannot find Guesser"

        elClass "div" "text-green-500" $ text $ "RELATED WORDS: " <> tShow rWords

teamColor :: Team -> T.Text
teamColor Blue = "text-blue-600"
teamColor Red = "text-red-600"

openRoomInfoDialog :: CodewordsM t m => Event t Bool -> Int
  -> NonEmptyText -> m (Event t (Maybe ClientMsg))
openRoomInfoDialog eToggle n (NonEmptyText initialRoomName) = mdo
  dOpen <- holdDyn False $ leftmost [eToggle, False <$ eClose]
  let mkStyles b = T.intercalate " "
        ["absolute flex justify-center \
          \ backdrop-filter backdrop-blur-sm items-center inset-0"
        , bool "opacity-0 pointer-events-none"
          "opacity-100 pointer-events-auto" b]
-- TODO: inputs clear most times
  eClose <- elDynClass "div" (mkStyles <$> dOpen) $ mdo
    latestBtnE <- elClass "div" "bg-red-500 w-1/2 h-1/2" $ do
      roomNameIE <- inputMNonEmptyTextBox "" initialRoomName "" latestBtnE
      passwordIE <- inputPasswordWidget "Room Pass: " ""
        defaultPasswordSymbols latestBtnE
      let btnStyles = "ml-2 px-1 bg-gray-600 rounded-lg"
      (saveBtn, _) <- elClass' "span" "" $
        btnWidget btnStyles "Save"
      (cancelBtn, _) <- elClass' "span" "" $
        btnWidget btnStyles "Cancel"

      let dMsg = (f n) <$> roomNameIE <*> passwordIE
          saveME = tag (current dMsg) $ domEvent Click saveBtn
          cancelE = domEvent Click cancelBtn
      return $ leftmost [saveME, Nothing <$ cancelE]
    return latestBtnE
  return eClose

  where f :: Int -> Maybe NonEmptyText -> ClientPassword -> Maybe ClientMsg
        f _ _ (ClientPassword _ (Just (NonEmptyText "•••••••"))) = Nothing
        f i mName mPass = flip (ChangeRoomInfo i) mPass <$> mName

handleChangeRoomInfo :: CodewordsM t m => Int -> Dynamic t ClientRoom
  -> m (Event t ClientMsg)
handleChangeRoomInfo n dR = do
  btn <- elClass "span" "" $
    btnWidget "ml-2 px-1 bg-gray-600 rounded-lg" "Change"
  cMMsg <- dyn $ (openRoomInfoDialog (True <$ btn) n) <$> (_clientRName <$> dR)

  cMMsg' <- switchHold never cMMsg
  let cMsg = fmapMaybe id $ cMMsg'
  return cMsg


displayRoom :: CodewordsM t m => Int -> Dynamic t ClientRoom -> User
  -> RoomStateView -> m (Event t ClientMsg)
displayRoom n r u v = do
  r' <- holdUniqDyn r
  elClass "div" "flex flex-col h-screen select-none" $ do
    let rAdminID = fmap _clientRAdminID r'
    let adminStatus = isUserAdmin rAdminID (return u)

    displayTopMessageBar r'

    elClass "div" "flex flex-col text-5xl md:flex-row h-full overflow-hidden lg:text-3xl" $ do
      btn <- elClass "div" "flex flex-col flex-grow bg-gray-500 rounded h-full" $ do
        case v of
          LobbyView -> do
            eRN' <- elClass "div" "flex" $ do
              eRN <- elDynClass "div" "p-1 flex-grow bg-gray-700 rounded-t" $ do
                dynText $ fmap (\room -> ("Room: " <> getNonEmptyText (_clientRName room))) r'
                displayIfAdmin adminStatus (handleChangeRoomInfo n r') (return never)

              --elClass "div" "lg:px-10 2xl:px-3 bg-gray-600 rounded-full" $ text "?"
              return eRN

            displayRoomUsers rAdminID (fmap _clientRPlayers r')

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

      cMsg <- elClass "div" "flex flex-col h-full md:w-1/4" $ do
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


displayRoomCard :: CodewordsM t m => Dynamic t (Int, ClientRoom)
  -> Maybe Int -> m (Event t ClientMsg)
displayRoomCard dR mRID = do
  e <- elClass "div" "bg-gray-400 p-4 rounded-lg" $ do
    let dRID = fst <$> dR
        dRName = (_clientRName.snd) <$> dR
        joinBtn = elClass' "button" "bg-gray-200 rounded-sm" $ text "Join"
        divStyle = "flex flex-row gap-x-2 flex-wrap"
    dyn $ ffor (_clientRPassReq.snd <$> dR) $ \case
      False ->
        elClass "div" divStyle $ do
          elClass' "span" "" $ dynText (getNonEmptyText <$> dRName)
          (btnE, _) <- joinBtn
          return $ tag (current ((flip JoinRoom Nothing) <$> dRID)) $ domEvent Click btnE
      True -> do
        --let dStyle = bool ("") ("ring-2 ring-red-500") <$>
        --      ((f' mRID) <$> dRID)
        elClass "div" divStyle $ mdo
          --elAttr "img" ("src" =: static @"lockSymbol.svg") blank
          elClass "span" "" $ dynText (getNonEmptyText <$> dRName)
          dPass <- inputMNEPassword "" "" $ domEvent Click btnE--dStyle
          (btnE, _) <- joinBtn
          dyn_ $ bool (return ()) (elClass "span" "bg-red-200" $ text "Password is invalid") <$>
            ((f' mRID) <$> dRID)
          let dMClientMsg = f <$> dRID <*> (_mPassword <$> dPass)
              dMsg = fmapMaybe id $ tag (current dMClientMsg) $ domEvent Click btnE
          return dMsg
  e' <- switchHold never e
  return e'
  where f :: Int -> Maybe NonEmptyText -> Maybe ClientMsg
        f i (Just p) = Just $ JoinRoom i (Just p)
        f _ Nothing = Nothing

        f' :: Maybe Int -> Int -> Bool
        f' Nothing _ = False
        f' (Just i') i = (==) i i'

displayRoomUsers :: CodewordsM t m => Dynamic t UserID
  -> Dynamic t (NonEmpty User) -> m ()
displayRoomUsers adminID users = elClass "div" "px-4 h-full" $ do
  elClass "div" "flex justify-center py-2" $ text "Players"
  elClass "div" "grid grid-cols-2 gap-4 px-2" $
    simpleList (fmap (reverse.NE.toList) users) (\u -> do
        let adminStatus = isUserAdmin adminID u
        let defaultStyle = "px-2 bg-gray-400 rounded"
        elDynClass "div" (T.append <$> (defaultStyle) <*>
          (fmap makeAdminStyle adminStatus)) $ dynText
            $ getNonEmptyText <$> fmap _name u
      )
  return ()
  where makeAdminStyle True  = " text-yellow-300"
        makeAdminStyle False = ""

roomsWidget :: CodewordsM t m => Event t (IntMap ClientRoom)
  -> Event t (Int, ClientRoom) -> Event t Int -> m (Dynamic t (IntMap ClientRoom))
roomsWidget rList e deleteEvent =
  foldDyn ($) mempty $ leftmost [ fmap mappend rList
  , fmap (\(i, r) -> insert i r) e
  , fmap delete deleteEvent]


roomListWidget :: CodewordsM t m => Dynamic t (IntMap ClientRoom)
  -> Maybe Int -> m (Event t ClientMsg)
roomListWidget rList mRID = do
  eList  <- elClass "div" "flex flex-col gap-y-2 overflow-y-auto" $ simpleList (fmap M.toList rList) (flip displayRoomCard mRID)
  return $ (switchDyn $ leftmost <$> eList)

----- INPUT TEXTBOX FNS

inputPasswordWidget :: CodewordsM t m => T.Text -> T.Text
  -> T.Text -> Event t a -> m (Dynamic t ClientPassword)
inputPasswordWidget label style initialValue eClear = do
  checkE' <- elClass "div" "py-2" $ do
    checkE <- inputElement $ def
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes
      .~ ("autofocus" =: "" <> "style" =: style
        <> "type" =: "checkbox")
    elClass "span" "pl-2" $ text "Require a password"
    return checkE
  let dChecked = _inputElement_checked checkE'
  -- eDCP :: Event t (Dynamic t ClientPassword)
  eDCP <- dyn $ ffor dChecked $ \case
    True -> do
      elClass "div" "flex flex-row py-2" $ do
        elClass "span" "pr-2" $ text label
        inputMNEPassword style initialValue eClear
    False -> return $ constDyn (ClientPassword False Nothing)
  join <$> holdDyn (constDyn $ ClientPassword False Nothing) eDCP

inputMNEPassword :: CodewordsM t m => T.Text -> T.Text
  -> Event t a -> m (Dynamic t ClientPassword)
inputMNEPassword style initialValue eClear = do
  iE <- inputElement $ def
    & inputElementConfig_elementConfig
    . elementConfig_initialAttributes
    .~ ("autofocus" =: "" <> "style" =: style
      <> "placeholder" =: "Type password here..."
      <> "type" =: "password")
    & inputElementConfig_setValue .~ (initialValue <$ eClear)

  let iEValue = _inputElement_value iE
  dPassChanged <- holdDyn False $ True <$ domEvent Keypress iE
  return $ ClientPassword <$> dPassChanged
    <*> (mkNonEmptyText <$> iEValue)


inputMNonEmptyTextBoxLabel :: CodewordsM t m => T.Text -> T.Text
  -> T.Text -> Event t a -> m (Dynamic t (Maybe NonEmptyText))
inputMNonEmptyTextBoxLabel labelName style placeholder eClear = do
  elClass "div" "flex flex-col" $ do
    el "span" $ text labelName
    inputMNonEmptyTextBox style "" placeholder eClear

inputMNonEmptyTextBox :: CodewordsM t m => T.Text -> T.Text
  -> T.Text -> Event t a -> m (Dynamic t (Maybe NonEmptyText))
inputMNonEmptyTextBox style initialValue placeholder eClear = do
  (elVal, _) <- inputTextBox style initialValue placeholder eClear
  return $ mkNonEmptyText <$> elVal

inputTextBox :: CodewordsM t m => T.Text -> T.Text -> T.Text -> Event t a
  -> m (Dynamic t T.Text, Event t ())
inputTextBox style initialValue placeholder eClear = do
  iE <- inputElement $ def
    & inputElementConfig_elementConfig
    . elementConfig_initialAttributes
    .~ ("autofocus" =: "" <> "style" =: style
      <> "placeholder" =: placeholder)
    & inputElementConfig_setValue .~ (initialValue <$ eClear)
  return (_inputElement_value iE,
    keypress Enter (_inputElement_element iE))

inputTextBoxBtnWidget :: CodewordsM t m
  => T.Text -> T.Text -> m (Event t T.Text)
inputTextBoxBtnWidget btnText style = mdo
  (elVal, enter) <- inputTextBox style "" "Send a message" send
  (e, _) <- elClass' "button" "bg-gray-100" $ text btnText

  let clicked = domEvent Click e
      send = leftmost [clicked, enter]

  e' <- dyn $ ffor ((/=) "SYSTEM" <$> elVal) $ \case
    False -> return never
    True -> return $ tag (current elVal) send
  switchHold never e'

signInWidget :: CodewordsM t m => m (Event t ClientMsg)
signInWidget = do
  nonEmptyText <- elClass "div" "flex flex-col justify-center w-screen h-screen text-5xl bg-gray-400" $ do
    elClass "div" "flex flex-col self-center w-3/4 p-8 rounded-lg bg-gray-500" $ do
      dMNonEmptyText <- inputMNonEmptyTextBox "" "" "Create a user name..." never
      (btnE, _) <- elClass' "button" "bg-gray-200" $ text "Create User"
      let eventNonEmptyText = fmapMaybe id $ tag (current dMNonEmptyText) $ domEvent Click btnE
      return $ eventNonEmptyText
  return $ CreateName <$> nonEmptyText

homeViewWidget :: CodewordsM t m => Dynamic t (IntMap ClientRoom) -> User
  -> Maybe Int -> m (Event t ClientMsg)
homeViewWidget rList u mRID = do
  elClass "div" "flex flex-col mx-16 gap-y-4 max-h-screen" $ do
    elClass "div" "flex self-center py-2 text-5xl font-bold" $ text $ "Welcome " <> (getNonEmptyText $ _name u) -- <> " ID: " <> tShow (_userID u)
    dMsg' <- elClass "div" "flex flex-col p-8 text-2xl bg-gray-500 rounded-lg" $ do
      elClass "div" "text-3xl pb-2 font-semibold" $ text "Create a new room:"
      dMName <- inputMNonEmptyTextBoxLabel "" "" "Enter a room name..." never
      dMCPass <- inputPasswordWidget "Room Pass: " "" "" never
      (btnE, _) <- elClass' "button" "bg-gray-200 rounded-lg font-semibold" $ text "Create Room"


      let dMMsg = liftA2 f dMName (_mPassword <$> dMCPass)
          dMsg = fmapMaybe id $ tag (current dMMsg) $ domEvent Click btnE
      return dMsg

    e <- elClass "div" "flex flex-col p-8 text-2xl bg-gray-500 rounded-lg overflow-y-auto" $ do
      elClass "div" "text-3xl pb-2 font-semibold" $ text "Join a room:"
      roomListWidget rList mRID

    return $ leftmost [e, dMsg']

  -- in the end what we want is Maybe (CreateRoom NonEmptyText (Maybe NonEmptyText))
  where f :: Maybe NonEmptyText -> Maybe NonEmptyText -> Maybe ClientMsg
        f mName mPass = flip CreateRoom mPass <$> mName

changeView :: ServerMsg -> View -> View
changeView (GameStarted rID) (LoggedIn u _) = LoggedIn u $ RoomView rID GameView
--changeView (GameStateChanged rID) (LoggedIn u _) = LoggedIn u $ RoomView rID GameView
changeView (LeftRoom u) _ = LoggedIn u $ HomeView Nothing
changeView (NameCreated u) _ = LoggedIn u $ HomeView Nothing
changeView (PasswordInvalid u rID) _ = LoggedIn u $ HomeView (Just rID)
changeView (RoomCreated rID _) (LoggedIn u _) = LoggedIn u $ RoomView rID LobbyView
changeView (GameEnded rID) (LoggedIn u _) = LoggedIn u $ RoomView rID LobbyView
-- TODO: Allow user to join mid game possibly?
changeView (RoomJoined rID) (LoggedIn u _) = LoggedIn u $ RoomView rID LobbyView
changeView _ v = v

router :: CodewordsM t m => Dynamic t (IntMap ClientRoom) -> View
  -> m (Event t ClientMsg)
router _ SignIn = signInWidget
router r (LoggedIn u (HomeView mRID)) = homeViewWidget r u mRID
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

      elAttr "link" ("href" =: "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css"
        <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css"
        <> "rel" =: "stylesheet") blank

      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, \
        \initial-scale=1") blank
  , _frontend_body = elClass "div" "w-screen h-screen bg-gray-400 overflow-hidden" $ do

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
