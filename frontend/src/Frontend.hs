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
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.IntMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Foldable as F
import qualified Data.IntMap as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle


import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

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
  | RoomView Int
  deriving Eq


codeWordsSocket :: CodewordsM t m
  => Event t ClientMsg -> m (Event t ServerMsg)
codeWordsSocket send = do
  rawWebsocket <- jsonWebSocket "ws://localhost:8000/websocket" $ def
    & webSocketConfig_send .~ (fmap pure send)

  return $ fmapMaybe id $ _webSocket_recv rawWebsocket


showMsgs :: (Reflex t, MonadHold t m, MonadFix m
  , DomBuilder t m, PostBuild t m)
  => Event t T.Text -> m ()
showMsgs msg = do
  msgs <- foldDyn (:) [] msg
  simpleList msgs $ el "div".dynText
  return ()

roomChatWidget :: CodewordsM t m => Dynamic t Room -> User -> m (Event t RoomChatMessage)
roomChatWidget r u = do
  elClass "div" "bg-gray-300 flex flex-col flex-grow break-all \
    \ overflow-auto h-1/3 lg:h-full" $
    simpleList (fmap (reverse._roomChat) r) (\rChat -> do
      el "div" $ do
        let user = fmap userSpeaking rChat
        elClass "span" "text-red-600" $ dynText $ fmap name user
        dynText ": "
        dynText $ fmap chatMessage rChat
      )
  elClass "div" "flex flex-row" $ do
    eventText <- inputTextBoxBtnWidget "Send" "width:85%"
    return $ fmap (RoomChatMessage u) eventText


btnWidget :: CodewordsM t m => T.Text -> T.Text -> m (Event t ())
btnWidget style t = do
  (e, _)  <- elClass' "button" style $ text t
  return $ domEvent Click e

displayRoom :: CodewordsM t m => Int -> Dynamic t Room -> User
  -> m (Event t ClientMsg)
displayRoom n r u = do
  elClass "div" "flex flex-col h-screen" $ do
    let roomAdmin = fmap _roomAdmin r
    let adminStatus = isUserAdmin roomAdmin (return u)
    elClass "div" "flex justify-center bg-gray-700 xl:h-16 \
      \ 2xl:h-8 rounded lg:text-5xl" $
      text "GET UR BEEG YOSHI HERE"
    elDynClass "div" "flex whitespace-pre-wrap \
      \ justify-center bg-gray-600 h-auto lg:text-3xl" $ do
      -- How would Sky do this
      let adminName = fmap name roomAdmin
      text "Waiting on "
      elDynClass "span" "text-yellow-300" $ dynText adminName
      text " to start the game"

    elClass "div" "flex flex-col text-5xl md:flex-row h-full overflow-hidden lg:text-3xl" $ do
      btn <- elClass "div" "flex flex-col flex-grow bg-gray-500 rounded" $ do
        elClass "div" "flex" $ do
          elDynClass "div" "p-1 flex-grow bg-gray-700 rounded-t" $ do
            dynText $ fmap (\room -> "Room: " <> _roomName room) r
            elClass "button" "ml-2 px-1 bg-gray-600 rounded-lg" $ text "Change"
          elClass "div" "lg:px-10 2xl:px-3 bg-gray-600 rounded-full" $ text "?"

        displayRoomUsers roomAdmin (fmap _roomPlayers r)

        elClass "div" "flex flex-row justify-between h-1/6" $ do
          let btnStyles = "m-2 p-1 bg-gray-600 rounded-lg"
          backBtn <- btnWidget btnStyles "Back"

          startBtn <- displayIfAdmin adminStatus
            (btnWidget btnStyles "Start") (return never)
          startBtn' <- switchHold never startBtn

          return $ leftmost [ LeaveRoom n <$ backBtn
            , StartGame n <$ startBtn']

      elClass "div" "flex flex-col lg:w-1/4" $ do
        msgStream <- roomChatWidget r u
        return $ leftmost [fmap (SendRoomChatMsg n) msgStream, btn]

isUserAdmin :: Reflex t => Dynamic t User
  -> Dynamic t User -> Dynamic t Bool
isUserAdmin admin u = (==) <$> (fmap userID u) <*> (fmap userID admin)

displayIfAdmin :: CodewordsM t m => Dynamic t Bool -> m b -> m b
  -> m (Event t b)
displayIfAdmin adminStatus adminAction defAction
  = dyn $ fmap doFn adminStatus
  where doFn False = defAction
        doFn True  = adminAction


displayRoomCard :: CodewordsM t m => Dynamic t (Int, Room) -> m (Event t Int)
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
          (fmap makeAdminStyle adminStatus)) $ dynText $ fmap name u
      )
  return ()
  where makeAdminStyle True  = " text-yellow-300"
        makeAdminStyle False = ""

roomsWidget :: CodewordsM t m => Event t (IntMap Room) -> Event t (Int, Room)
  -> Event t Int -> m (Dynamic t (IntMap Room))
roomsWidget rList e deleteEvent =
  foldDyn ($) mempty $ leftmost [ fmap mappend rList
  , fmap (\(i, r) -> insert i r) e
  , fmap delete deleteEvent]
{-  (\(roomID, room) currentMap ->
    insert roomID room currentMap
  ) mempty e
-}

roomListWidget :: CodewordsM t m => Dynamic t (IntMap Room) -> m (Event t Int)
roomListWidget rList = do
  eList <- el "div" $ simpleList (fmap M.toList rList) displayRoomCard
  return $ switchDyn $ leftmost <$> eList


inputTextBoxBtnWidget :: CodewordsM t m
  => T.Text -> T.Text -> m (Event t T.Text)
inputTextBoxBtnWidget btnText style = do
  inputTextBox <- inputElement $ def
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
    .~ ("autofocus" =: "" <> "style" =: style )
      -- Looks like works for a second?
  (e, _) <- elClass' "button" "bg-gray-100" $ text btnText
  let val = _inputElement_value inputTextBox
      clicked = domEvent Click e
      enter = keypress Enter (_inputElement_element inputTextBox)
  return $ tag (current val) (leftmost [clicked, enter])

signInWidget :: CodewordsM t m => m (Event t ClientMsg)
signInWidget = do
  elClass "div" "text-5xl" $ do
    eventText <- inputTextBoxBtnWidget "Choose this name" ""
    return $ fmap CreateName eventText

homeViewWidget :: CodewordsM t m => Dynamic t (IntMap Room) -> User -> m (Event t ClientMsg)
homeViewWidget rList user = do
  el "div" $ text $ "Welcome " <> name user <> " ID: " <> tShow (userID user)
  e <- inputTextBoxBtnWidget "Create Room" ""
  roomClicked <- roomListWidget rList
  return $ leftmost [fmap JoinRoom roomClicked, fmap (flip CreateRoom Nothing) e]

changeView :: ServerMsg -> View -> View
changeView (LeftRoom u) _ = LoggedIn u HomeView
changeView (NameCreated u) _ = LoggedIn u HomeView
changeView (RoomCreated roomID _) (LoggedIn u _) = LoggedIn u $ RoomView roomID
changeView (RoomJoined roomID) (LoggedIn u _) = LoggedIn u $ RoomView roomID
changeView _ v = v

router :: CodewordsM t m => Dynamic t (IntMap Room) -> View -> m (Event t ClientMsg)
router _ SignIn = signInWidget
router r (LoggedIn u HomeView) = homeViewWidget r u
router roomList (LoggedIn u (RoomView n)) = do
  let currentRoom = M.lookup n <$> roomList
  currentRoom' <- maybeDyn currentRoom
  clientMsg <- dyn $ ffor currentRoom' $ \case
    Just r -> displayRoom n r u
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
        serverMsg <- codeWordsSocket clientMsg

        currentView <- foldDyn changeView SignIn serverMsg
        currentView' <- holdUniqDyn currentView
        newServerMsg <- dyn $ router roomList <$> currentView'
        clientMsg <- switchHold never newServerMsg

        roomList <- roomsWidget (fmapMaybe (preview _RoomList) serverMsg)
          (leftmost [fmapMaybe (preview _RoomChanged) serverMsg
          , fmapMaybe (preview _RoomCreated) serverMsg])
          $ fmapMaybe (preview _RoomDeleted) serverMsg
        -- displays all rooms --simpleList (fmap elems roomList) displayRoomCard
        --showMsgs name
        return ()
        --randomBoard <- liftIO $ newGame ["Bob", "your uncle", "blah", "turd", "123124", "zcdsa"]
        --text $ T.pack.show $ randomBoard

      return ()
  }
