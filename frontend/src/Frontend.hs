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

data LoggedInView
  = HomeView
  | RoomView Int

noStyle :: Style
noStyle = ""

type Style = T.Text

roomChatStyle :: [Style]
roomChatStyle = [ "background-color: lightgray"
  , "border-top-left-radius: 10px"
  , "border-top-right-radius: 10px"
  , "font-style: italic"
  , "min-height: 4em"
  , "padding: 10px" ]

combineStyles :: [Style] -> Style
combineStyles t = F.foldr (\x xs -> x <> ";" <> xs) "" t
-- theres prob a fold with fn? and should i be using another variant

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
  elAttr "div" ("style" =: (combineStyles roomChatStyle)) $
    simpleList (fmap (reverse._roomChat) r) (\rChat -> do
      el "div" $ do
        let user = fmap userSpeaking rChat
        elClass "span" "text-red-600" $ dynText $ fmap name user
        dynText ": "
        dynText $ fmap chatMessage rChat
      )
  elClass "div" "flex" $ do
    eventText <- inputTextBoxBtnWidget "Send" "flex: auto"
    return $ fmap (RoomChatMessage u) eventText

displayRoom :: CodewordsM t m => Int -> Dynamic t Room -> User
  -> m (Event t ClientMsg)
displayRoom n r u = do
  elClass "div" "flex flex-row" $ do
    displayRoomUsers $ fmap _roomPlayers r
    elClass "div" "flex-grow" $ do
      dynText $ fmap (\room -> "Room name: " <> _roomName room) r
      msgStream <- roomChatWidget r u
      return $ fmap (SendRoomChatMsg n) msgStream

displayRoomCard :: CodewordsM t m => Dynamic t (Int, Room) -> m (Event t Int)
displayRoomCard r = do
  (e, _) <- el' "div" $ dynText $ fmap (_roomName.snd) r
  return $ tag (current $ fmap fst r) $ domEvent Click e

displayRoomUsers :: CodewordsM t m => Dynamic t (NonEmpty User) -> m ()
displayRoomUsers users = elClass "div" "flex-grow" $ do
  el "div" $ text "Users in room: "
  simpleList (fmap NE.toList users) (\u -> el "div" $ dynText $ fmap name u)
  return ()

roomsWidget :: CodewordsM t m => Event t (IntMap Room) -> Event t (Int, Room)
  -> m (Dynamic t (IntMap Room))
roomsWidget rList e =
  foldDyn ($) mempty $ leftmost [ fmap mappend rList
  , fmap (\(i, r) -> insert i r) e]
{-  (\(roomID, room) currentMap ->
    insert roomID room currentMap
  ) mempty e
-}

roomListWidget :: CodewordsM t m => Dynamic t (IntMap Room) -> m (Event t Int)
roomListWidget rList = do
  eList <- el "div" $ simpleList (fmap M.toList rList) displayRoomCard
  return $ switchDyn $ leftmost <$> eList


inputTextBoxBtnWidget :: CodewordsM t m
  => T.Text -> Style -> m (Event t T.Text)
inputTextBoxBtnWidget btnText style = do
  inputTextBox <- inputElement $ def
    & inputElementConfig_elementConfig
    . elementConfig_initialAttributes
    .~ ("style" =: style)
  (e, _) <- el' "button" $ text btnText
  let val = _inputElement_value inputTextBox
      clicked = domEvent Click e
      enter = keypress Enter (_inputElement_element inputTextBox)

  return $ tag (current val) (leftmost [clicked, enter])

signInWidget :: CodewordsM t m => m (Event t ClientMsg)
signInWidget = do
  eventText <- inputTextBoxBtnWidget "Choose this name" noStyle
  return $ fmap CreateName eventText

homeViewWidget :: CodewordsM t m => Dynamic t (IntMap Room) -> User -> m (Event t ClientMsg)
homeViewWidget rList user = do
  el "div" $ text $ "Welcome " <> name user <> " ID: " <> tShow (userID user)
  e <- inputTextBoxBtnWidget "Create Room" noStyle
  roomClicked <- roomListWidget rList
  return $ leftmost [fmap JoinRoom roomClicked, fmap (flip CreateRoom Nothing) e]

changeView :: ServerMsg -> View -> View
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
      elAttr "link" ("href" =: "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = mdo

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
        newServerMsg <- dyn $ router roomList <$> currentView
        clientMsg <- switchHold never newServerMsg

        roomList <- roomsWidget (fmapMaybe (preview _RoomList) serverMsg) $
          leftmost [fmapMaybe (preview _RoomChanged) serverMsg
          , fmapMaybe (preview _RoomCreated) serverMsg]
        -- displays all rooms --simpleList (fmap elems roomList) displayRoomCard
        --showMsgs name
        return ()
        --randomBoard <- liftIO $ newGame ["Bob", "your uncle", "blah", "turd", "123124", "zcdsa"]
        --text $ T.pack.show $ randomBoard

      return ()
  }
