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

import Data.IntMap (IntMap, insert, delete)
import Data.Char (isAlpha)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.IntMap as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Language.Javascript.JSaddle

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

codeWordsSocket :: CodewordsM t m
  => Event t ClientMsg -> m (Event t ServerMsg)
codeWordsSocket send = do
  {-
    pure :: a -> f a
      Lift a value.

    return :: a -> m a
      Inject a value into the monadic type.
  -}
  rawWebsocket <- jsonWebSocket "ws://localhost:8000/websocket" $ def
    & webSocketConfig_send .~ (fmap pure send)

  return $ fmapMaybe id $ _webSocket_recv rawWebsocket


showMsgs :: (MonadHold t m, MonadFix m
  , DomBuilder t m, PostBuild t m)
  => Event t T.Text -> m ()
showMsgs msg = do
  {-
  foldDyn :: hoogle => (a -> b -> b) -> b -> Event t a
    -> m (Dynamic t b)
    Create a Dynamic using the initial value and change
    it each time the Event occurs using a folding function
    on the previous value and the value of the Event.
  -}
  msgs <- foldDyn (:) [] msg
  simpleList msgs $ el "div" . dynText
  return ()

roomChatWidget :: CodewordsM t m => User -> Dynamic t Room
  -> m (Event t RoomChatMessage)
roomChatWidget u r = do
  {-
  data Dynamic t :: * -> *
    A container for a value that can change over time and
    allows notifications on changes. Basically a combination
    of a Behavior and an Event, with a rule that the
    Behavior will change if and only if the Event fires.

  data Behavior t :: * -> *
    A container for a value that can change over time.
    Behaviors can be sampled at will, but it is not possible
    to be notified when they change.

  data Event t :: * -> *
    A stream of occurrences. During any given frame, an
    Event is either occurring or not occurring; if it is
    occurring, it will contain a value of the given type
    (its "occurrence type")

  simpleList :: hoogle => Dynamic t [v]
    -> (Dynamic t v -> m a) -> m (Dynamic t [a])
    Create a dynamically-changing set of widgets from a
    Dynamic list.

  dynText :: hoogle => Dynamic t Text -> m ()

  elDynClass' :: hoogle => Text -> Dynamic t Text -> m a
   -> m (Element EventResult (DomBuilderSpace m) t, a)
   Create a DOM element with a Dynamic class and return the element
  -}
  elClass "div" "bg-gray-300 flex flex-col-reverse h-1/3 lg:h-full overflow-auto" $
    elClass "div" "flex flex-col break-all \
      \ " $
      simpleList (fmap (reverse._roomChat) r) (\rChat -> do
        el "div" $ do
          let uS = fmap userSpeaking rChat
          elClass "span" "text-red-600" $ dynText $ fmap _name uS
          dynText ": "
          dynText $ fmap chatMessage rChat
        )

  e <- dyn $ ffor (_roomGameState <$> r) $ \case
    Just gs -> do
      let currentP = getPlayer u gs
      case currentP of
        Just p -> ifSpeaker $ _role p
        Nothing -> showInput
    Nothing -> showInput
  switchHold never e

  where showInput = elClass "div" "flex flex-row" $ do
          eventText <- inputTextBoxBtnWidget
            "Send" "width:85%"
          return $ fmap (RoomChatMessage u) eventText

        ifSpeaker Speaker = elClass "div" "flex flex-row" $
            text "Speakers can't talk" >> return never
        ifSpeaker _ = showInput


styleCard :: PlayerRole -> Codeword -> T.Text
styleCard pr (Codeword _ o r)
    | r == True = T.pack $ (++) (color o) "700"
    | pr == Speaker = T.pack $ (++) (color o) "300"
    | otherwise = T.pack $ "bg-gray-400"
    where color Bystander = "bg-green-"
          color Killer = "bg-gray-"
          color (TeamOwned Red) = "bg-red-"
          color (TeamOwned Blue) = "bg-blue-"


displayRoleBar :: CodewordsM t m => User
  -> Dynamic t (Maybe GameState) -> m ()
displayRoleBar u mGS = do
  elClass "div" "flex justify-between" $ do
    dyn_ $ ffor mGS $ \case
      Just gs -> do
        elClass "div" "" $ do
          text "Current Turn: "
          elClass "span" (teamColor $ _currentTurn gs) $
            text $ tShow $ _currentTurn gs

        let currentPlayer = getPlayer u gs

        case currentPlayer of
          Just p -> do
            elClass "div" "" $ do
              text "Your Role: "
              elClass "span" (teamColor $ _team p) $
                text $ tShow $ _team p
              el "span" $ text $ T.pack $ "'s " ++ (show $ _role p)

          Nothing -> do
            el "span" $ text "Couldnt find player"
            return ()

      Nothing -> do
        el "span" $ text "Gamestate does not exist:("
        return ()

gameBoardUI :: CodewordsM t m => Player -> Dynamic t TurnPhase
  -> Dynamic t Board -> Dynamic t Team -> m (Event t Codeword)
gameBoardUI p tP gb t = elClass "div" "grid grid-cols-5 gap-4 p-4 h-full" $ do
  eList <- simpleList gb (codewordUI p t tP)
  return $ switchDyn $ leftmost <$> eList

codewordUI :: CodewordsM t m => Player -> Dynamic t Team
  -> Dynamic t TurnPhase -> Dynamic t Codeword -> m (Event t Codeword)
codewordUI p t' tP c = do
  (e, _) <- elDynClass' "div" (mkStyle <$> c) $ dynText
    $ _word <$> c

  clickE <- dyn $ (isGuesser p c e) <$> tP <*> t'
  switchHold never clickE

  where isGuesser (Player Guesser pT _) c' e' (Guessing _) tTurn =
          doIfTrue c' e' $ tTurn == pT
        isGuesser _ _ _ _ _ = return never

        doIfTrue c2 e2 True = return $ tag (current c2) $ domEvent Click e2
        doIfTrue _ _ False = return never

        mkStyle c' = "p-4 flex justify-center rounded-lg items-center "
                <> styleCard (_role p) c'

displayCodewords :: CodewordsM t m => User
 -> Dynamic t GameState -> m (Event t Codeword)
displayCodewords u gs = do
  let mP = getPlayer u <$> gs
  clientMsg <- dyn $ ffor mP $ \case
    Just p -> gameBoardUI p (_turnPhase <$> gs) (_gameBoard <$> gs) (_currentTurn <$> gs)
    Nothing -> el "div" $ text "err at displayCodewords" >> return never
  switchHold never clientMsg

displayGameBoard :: CodewordsM t m => User
  -> Dynamic t (Maybe GameState) -> m (Event t Codeword)
displayGameBoard u mGS = do
  {-
  forM :: hoogle => t a -> (a -> m b) -> m (t b)
    forM is mapM with its arguments flipped.
    For a version that ignores the results
    see forM_.
    mapM: Map each element of a structure to a
    monadic action, evaluate these actions
    from left to right, and collect the
    results. For a version that ignores
    the results see mapM_.

  pack :: String -> Text
    O(n) Convert a String into a Text.
    Subject to fusion. Performs
    replacement on invalid scalar values.

  leftmost :: Reflex t => [Event t a] -> Event t a
    Create a new Event that occurs if at
    least one of the Events in the list
    occurs. If multiple occur at the same
    time the value is the value of the
    leftmost event.

  domEvent :: EventName eventName -> target
    -> Event t (DomEventType target eventName)
  -}
  mGS' <- maybeDyn mGS
  codewordE <- dyn $ ffor mGS' $
    \case
      Just gs -> do
        displayCodewords u gs
      Nothing -> do
        el "div" $ text "Game could not start since \
          \ gamestate does not exist."
        return never
  switchHold never codewordE

displayBottomBar :: CodewordsM t m => User -> Int
  -> Dynamic t (Maybe GameState) -> m (Event t ClientMsg)
displayBottomBar _ n mGS = do
  elClass "div" "flex" $ do
    backBtn <- elClass "span" "" $
      btnWidget "bg-gray-600" "Leave"

    dyn_ $ ffor mGS $ \case
      Just gs -> do
        elClass "span" "flex flex-col text-base" $ do
          elClass "span" "text-center" $ text "CARDS LEFT"
          elClass "span" "" $ do
            elClass "span" "text-blue-600" $ text "BLUE: "
            elClass "span" "" $ text $ tShow $ _blueCards $ _cardsLeft gs

            elClass "span" "text-red-600" $ text "RED: "
            elClass "span" "" $ text $ tShow $ _redCards $ _cardsLeft gs

        ifGuessing $ _turnPhase gs

      Nothing -> return never

    return $ leftmost [LeaveRoom n <$ backBtn]

ifGuessing :: CodewordsM t m => TurnPhase -> m (Event t ())
ifGuessing (Guessing (Clue _ _ g)) = do
        elClass "span" "" $ text $ "GUESSES LEFT: " <> (tShow g)
        elClass "span" "" $ btnWidget "bg-gray-600" "END TURN"
        return never
ifGuessing _ = return never

gameBoardWidget :: CodewordsM t m => Int -> Dynamic t Room
  -> User -> m (Event t ClientMsg)
gameBoardWidget n r u = do
    {-
  dyn_ :: hoogle => Dynamic t (m a) -> m ()
    Like dyn but discards result.
    dyn: Given a Dynamic of widget-creating
    actions, create a widget that is recreated
    whenever the Dynamic updates. The returned
    Event occurs whenever the child widget is
    updated, which is at post-build in
    addition to the times at which the input
    Dynamic is updated, and its value is the
    result of running the widget. Note: Often,
    the type a is an Event, in which case the
    return value is an Event-of-Events that
    would typically be flattened
    (via switchHold).

  (<$>) :: Functor f => (a -> b) -> f a -> f b
      An infix synonym for fmap.
  -}
  displayRoleBar u (_roomGameState <$> r)
  codeword <- displayGameBoard u (_roomGameState <$> r)
  bottomE <- displayBottomBar u n (_roomGameState <$> r)
  return $ leftmost [ChangeGameState n <$> codeword,
    bottomE]


btnWidget :: CodewordsM t m => T.Text -> T.Text -> m (Event t ())
btnWidget style t = do
  (e, _)  <- elClass' "button" style $ text t
  return $ domEvent Click e

displaySideBar :: CodewordsM t m => Int -> User
  -> Dynamic t Room -> m (Event t ClientMsg)
displaySideBar n u r = do
  e <- dyn $ ffor (_roomGameState <$> r) $ \case
    Just gs -> do
      let currentP = getPlayer u gs
          currentTeam = _currentTurn gs
          tPhase = _turnPhase gs
      case currentP of
        Just p -> do
          let pr = _role p
              pt = _team p
          ifSpeakerClue pr tPhase (currentTeam == pt) gs
        Nothing -> displayRoomChat n u r

    Nothing -> displayRoomChat n u r
  switchHold never e

  where ifSpeakerClue Speaker CluePicking True gs' = displaySpeakerView n (_gameBoard gs')
        ifSpeakerClue _ _ _ _ = displayRoomChat n u r

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
  -> Board -> m (Event t ClientMsg)
displaySpeakerView n gb = mdo
  elClass "div" "flex flex-col justify-between bg-gray-600 \
    \ h-full text-center" $ mdo

    elClass "div" "" $ text "Give your team a one word hint!"

    cMsg <- elClass "div" "" $ do
      elClass "div" "" $ text "HINT"
      h <- inputElement $ def

      let elVal = _inputElement_value h
          eitherClue = (parseHintInput gb) <$> elVal <*> guesses
      dyn $ ffor eitherClue $ \case
        Left a -> do
          elClass "div" "bg-red-300" $ text $ hintErrMsg a
          return never

        Right c ->
          return $ (SendClue n c) <$ (domEvent Click submitE)

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

          return $ leftmost [increment, decrement]

        return dInt'
      return g


    let btnColor = "bg-gray-400"
    (submitE, _) <- elClass' "button" btnColor $
      text "SUBMIT HINT"

    switchHold never cMsg

    where btn label fn = mdo
            (e, _) <- el' "button" $ text label
            return $ fn <$ domEvent Click e

          incFn 9 = 9
          incFn x = x + 1
          decFn 0 = 0
          decFn x = subtract 1 x


displayRoomChat :: CodewordsM t m => Int -> User
  -> Dynamic t Room -> m (Event t ClientMsg)
displayRoomChat n u r = do
  msgStream <- roomChatWidget u r
  return $ fmap (SendRoomChatMsg n) msgStream

displayTopMessageBar :: CodewordsM t m => Dynamic t Room
  -> m ()
displayTopMessageBar r = do
  elDynClass "div" "flex whitespace-pre-wrap \
    \ justify-center bg-gray-600 h-auto lg:text-3xl" $ do
    dyn_ $ ffor (_roomGameState <$> r) $ \case
      Just gs -> do
        case (_winner gs) of
          True -> do
            elClass "span" (teamColor $ _currentTurn gs) $
              text $ T.pack $ show $ _currentTurn gs
            text " team wins!"
          False ->
            turnPhaseMsg gs $ _turnPhase gs

      Nothing -> do
        -- TODO: How to do this differently?
        text "Waiting on "
        elDynClass "span" "text-yellow-300" $ dynText $
          _name._roomAdmin <$> r
        text " to start the game"

    return ()
  return ()

turnPhaseMsg :: CodewordsM t m => GameState -> TurnPhase -> m ()
turnPhaseMsg gs CluePicking = do
  case (getCurrentRole Speaker gs) of
    Just p -> do
      elClass "span" (teamColor $ _team p)
        $ text $ _name $ _user p
      text  " is thinking of a hint."
      return ()
    Nothing ->
      text "Cannot find Speaker"
turnPhaseMsg gs (Guessing (Clue h r _)) = do
  elClass "span" "flex justify-between w-full px-2" $ do
    el "div" $ do
      elClass "span" "color-red-600" $ text $ "HINT: " <> h

    el "div" $ do
      text "Guesser: "
      case (getCurrentRole Guesser gs) of
        Just p ->
          elClass "span" (teamColor $ _team p) $ text
            $ _name $ _user p
        Nothing ->
          text "Cannot find Guesser"

    el "div" $ text $ "RELATED WORDS: " <> tShow r

teamColor :: Team -> T.Text
teamColor Blue = "text-blue-600"
teamColor Red = "text-red-600"

displayRoom :: CodewordsM t m => Int -> Dynamic t Room -> User
  -> RoomStateView -> m (Event t ClientMsg)
displayRoom n r u v = do
  {-
  dynText :: hoogle => Dynamic t Text -> m ()
  -}
  elClass "div" "flex flex-col h-screen select-none" $ do
    let rAdmin = fmap _roomAdmin r
    let adminStatus = isUserAdmin rAdmin (return u)
    elClass "div" "flex justify-center bg-gray-700 xl:h-16 \
      \ 2xl:h-8 rounded lg:text-5xl" $
      text "GET UR BEEG YOSHI HERE"

    displayTopMessageBar r

    elClass "div" "flex flex-col text-5xl md:flex-row h-full overflow-hidden lg:text-3xl" $ do
      btn <- elClass "div" "flex flex-col flex-grow bg-gray-500 rounded h-full" $ do
        case v of
          LobbyView -> do
            elClass "div" "flex" $ do
              elDynClass "div" "p-1 flex-grow bg-gray-700 rounded-t" $ do
                dynText $ fmap (\room -> "Room: " <> _roomName room) r
                elClass "button" "ml-2 px-1 bg-gray-600 rounded-lg" $ text "Change"
              elClass "div" "lg:px-10 2xl:px-3 bg-gray-600 rounded-full" $ text "?"

            displayRoomUsers rAdmin (fmap _roomPlayers r)

            elClass "div" "flex flex-row justify-between h-1/6" $ do
              let btnStyles = "m-2 p-1 bg-gray-600 rounded-lg"
              backBtn <- btnWidget btnStyles "Leave"

              startBtn <- displayIfAdmin adminStatus
                (btnWidget btnStyles "Start") (return never)
              startBtn' <- switchHold never startBtn

              return $ leftmost [ LeaveRoom n <$ backBtn
                , StartGame n <$ startBtn']

          GameView -> do
            clientMsg <- gameBoardWidget n r u
            return clientMsg

      cMsg <- elClass "div" "flex flex-col h-full lg:w-1/4" $ do
        displaySideBar n u r

      return $ leftmost [cMsg, btn]

isUserAdmin :: Reflex t => Dynamic t User
  -> Dynamic t User -> Dynamic t Bool
  {-
  (<*>) :: f (a -> b) -> f a -> f b
    Sequential application. A few functors support an implementation
    of <*> that is more efficient than the default one.
    Using ApplicativeDo: 'fs <*> as' can be understood as
    the do expression:
      do f <- fs
         a <- as
         pure (f a)
  -}
isUserAdmin admin u = (==) <$> (fmap _userID u) <*> (fmap _userID admin)

displayIfAdmin :: CodewordsM t m => Dynamic t Bool -> m b -> m b
  -> m (Event t b)
  {-
  dyn :: hoogle => Dynamic t (m a) -> m (Event t a)
    Given a Dynamic of widget-creating actions, create a widget
    that is recreated whenever the Dynamic updates. The returned
    Event occurs whenever the child widget is updated, which is
    at post-build in addition to the times at which the input
    Dynamic is updated, and its value is the result of running
    the widget. Note: Often, the type a is an Event, in which
    case the return value is an Event-of-Events that would
    typically be flattened (via switchHold).
  -}
displayIfAdmin adminStatus adminAction defAction
  = dyn $ fmap doFn adminStatus
  where doFn False = defAction
        doFn True  = adminAction


displayRoomCard :: CodewordsM t m => Dynamic t (Int, Room)
  -> m (Event t Int)
  {-
  tag :: Reflex t => Behavior t b -> Event t a -> Event t b
    Replace each occurrence value of the Event with the value
    of the Behavior at the time of that occurrence.

  current :: Reflex t => Dynamic t a -> Behavior t a
    Extract the Behavior of a Dynamic.
  -}
displayRoomCard r = do
  (e, _) <- el' "div" $ dynText $ fmap (_roomName.snd) r
  return $ tag (current $ fmap fst r) $ domEvent Click e


displayRoomUsers :: CodewordsM t m => Dynamic t User
  -> Dynamic t (NonEmpty User) -> m ()
displayRoomUsers admin users = elClass "div" "px-4 h-full" $ do
  elClass "div" "flex justify-center py-2" $ text "Players"
  elClass "div" "grid grid-cols-2 gap-4 px-2" $
    simpleList (fmap (reverse.NE.toList) users) (\u -> do
        let adminStatus = isUserAdmin admin u
        let defaultStyle = "px-2 bg-gray-400 rounded"
        elDynClass "div" (T.append <$> (defaultStyle) <*>
          (fmap makeAdminStyle adminStatus)) $ dynText $ fmap _name u
      )
  return ()
  where makeAdminStyle True  = " text-yellow-300"
        makeAdminStyle False = ""

roomsWidget :: CodewordsM t m => Event t (IntMap Room)
  -> Event t (Int, Room) -> Event t Int -> m (Dynamic t (IntMap Room))
  {-
  mempty :: a
    Identity of mappend

  mappend :: a -> a -> a
    An associative operation

  (<>) :: a -> a -> a
    An associative operation.
    >>> [1,2,3] <> [4,5,6]
      [1,2,3,4,5,6]
  -}
roomsWidget rList e deleteEvent =
  foldDyn ($) mempty $ leftmost [ fmap mappend rList
  , fmap (\(i, r) -> insert i r) e
  , fmap delete deleteEvent]


roomListWidget :: CodewordsM t m => Dynamic t (IntMap Room)
  -> m (Event t Int)
  {-
  switchDyn :: forall t a. Reflex t =>
    Dynamic t (Event t a) -> Event t a
    Switches to the new Event whenever it receives one. Only the old event
    is considered the moment a new one is switched in; the output event
    will fire at that moment if only if the old event does.

  toList :: IntMap a -> [(Key, a)]
    Convert the map to a list of key/value pairs. Subject to list fusion.
  -}
roomListWidget rList = do
  eList <- el "div" $ simpleList (fmap M.toList rList) displayRoomCard
  return $ switchDyn $ leftmost <$> eList


inputTextBoxBtnWidget :: CodewordsM t m
  => T.Text -> T.Text -> m (Event t T.Text)
  {-
  (.~) :: ASetter s t a b -> b -> s -> t
    Replace the target of a Lens or all of the targets of a Setter
    or Traversal with a constant value.

  (<$) :: a -> f b -> f a
    Replace all locations in the input with the same value. The
    default definition is fmap . const, but this may be overridden
    with a more efficient version.

    Using ApplicativeDo: 'a <$ bs' can be understood as the do
    expression:
      do bs
         pure a
  -}
inputTextBoxBtnWidget btnText style = mdo
  inputTextBox <- inputElement $ def
    & inputElementConfig_elementConfig
    . elementConfig_initialAttributes
    .~ ("autofocus" =: "" <> "style" =: style )
    & inputElementConfig_setValue .~ ("" <$ send)

  (e, _) <- elClass' "button" "bg-gray-100" $ text btnText

  let clicked = domEvent Click e
      enter = keypress Enter (_inputElement_element
        inputTextBox)
      send = leftmost [clicked, enter]
      elVal = _inputElement_value inputTextBox

  return $ tag (current elVal) send

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
-- TODO: Allow user to join mid game possibly?
changeView (RoomJoined rID) (LoggedIn u _) = LoggedIn u $ RoomView rID LobbyView
changeView _ v = v

router :: CodewordsM t m => Dynamic t (IntMap Room) -> View
  -> m (Event t ClientMsg)
router _ SignIn = signInWidget
router r (LoggedIn u HomeView) = homeViewWidget r u
router roomList (LoggedIn u (RoomView n v)) = do
  {-
  maybeDyn :: hoogle => Dynamic t (Maybe a)
    -> m (Dynamic t (Maybe (Dynamic t a)))
    Factor a Dynamic t (Maybe a) into a Dynamic t (Maybe (Dynamic t a)),
    such that the outer Dynamic is updated only when the Maybe's
    constructor changes from Nothing to Just or vice-versa. Whenever
    the constructor becomes Just, an inner Dynamic will be provided,
    whose value will track the a inside the Just; when the constructor
    becomes Nothing, the existing inner Dynamic will become constant,
    and will not change when the outer constructor changes back to
    Nothing.

  (>>) :: forall a b. m a -> m b -> m b
    Sequentially compose two actions, discarding any value produced
    by the first, like sequencing operators (such as the semicolon)
    in imperative languages.

  never :: (Monad f, MonadFree f m) => m a
    A computation that never terminates

  ffor :: Functor f => f a -> (a -> b) -> f b
    Flipped version of fmap.

  switchHold :: hoogle => Event t a
    -> Event t (Event t a) -> m (Event t a)
    Switches to the new event whenever it receives one. Only the
    old event is considered the moment a new one is switched in;
    the output event will fire at that moment only if the old
    event does.

  lookup :: Key -> IntMap a -> Maybe a
    Lookup the value at a key in the map. See also lookup.
  -}
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
  , _frontend_body = elClass "div" "w-screen h-screen bg-gray-500" $ mdo

      {- Counter
      counter <- foldDyn (+) (0 :: Integer) $ leftmost [(1 <$ clickEvent), ((-1) <$ clickEventG)]
      (g, _) <- el' "button" $ text "sda"
      dynText $ T.pack.show <$> counter
      (e, _) <- el' "button" $ text "ads"
      let clickEvent = domEvent Click e
          clickEventG = domEvent Click g

      inputTextBox <- inputElement def
      dynText $ fmap T.reverse $ _inputElement_value inputTextBox
      --}

      prerender_ blank $ liftJSM $ void $ eval ("" :: T.Text)
      prerender_ blank $ mdo
        {-
          prerender_ :: (Functor m, Reflex t, Prerender js t m)
            => m () -> Client m () -> m ()
            Render the first widget on the server, and the
            second on the client. The hydration builder will
            run *both* widgets.

          liftJSM :: MonadJSM m => JSM a -> m a
            The liftJSM is to JSM what liftIO is to IO. When
            using GHCJS it is liftIO.

          liftIO :: IO a -> m a
            Lift a computation from the IO monad.

          eval :: ToJSString script => script
            -> JSM JSVAL
            JavaScript to evalute.

          holdUniqDyn :: hoogle => Dynamic t a -> m (Dynamic t a)
            Create a new Dynamic that only signals changes if the
            values actually changed.

          fmapMaybe :: Filterable f => (a -> Maybe b) -> f a -> f b
            Alias for mapMaybe: mapMaybe :: (a -> Maybe b) -> [a] -> [b]
            The mapMaybe function is a version of map which can
            throw out elements. In particular, the functional
            argument returns something of type Maybe b. If this
            is Nothing, no element is added on to the result list.
            If it is Just b, then b is included in the result list.

          preview :: MonadReader s m => Getting (First a) s a
            -> m (Maybe a)
            Retrieve the first value targeted by a Fold or Traversal
            (or Just the result from a Getter or Lens). See also
            firstOf and ^?, which are similar with some subtle
            differences (explained below).
        -}
        serverMsg <- codeWordsSocket clientMsg

        currentView <- foldDyn changeView SignIn serverMsg
        currentView' <- holdUniqDyn currentView
        newServerMsg <- dyn $ router roomList <$> currentView'
        clientMsg <- switchHold never newServerMsg

        roomList <- roomsWidget (fmapMaybe (preview _RoomList) serverMsg)
          (leftmost [fmapMaybe (preview _RoomChanged) serverMsg
          , fmapMaybe (preview _RoomCreated) serverMsg])
          $ fmapMaybe (preview _RoomDeleted) serverMsg

        return ()
      return ()
  }
