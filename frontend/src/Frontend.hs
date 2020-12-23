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
import qualified Data.IntMap as M
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

displayRoom :: CodewordsM t m => Dynamic t Room -> m ()
displayRoom r = el "div" $ dynText
  $ fmap (\room -> "Room name: " <> roomName room) r

displayRoomCard :: CodewordsM t m => Dynamic t Room -> m ()
displayRoomCard r = el "div" $ dynText $ fmap roomName r

roomsWidget :: CodewordsM t m => Event t (Int, Room)
  -> m (Dynamic t (IntMap Room))
roomsWidget e = foldDyn (\(roomID, room) currentMap ->
    insert roomID room currentMap
  ) mempty e


inputTextBoxBtnWidget :: CodewordsM t m => m (Event t T.Text)
inputTextBoxBtnWidget = do
  inputTextBox <- inputElement def
  (e, _) <- el' "button" $ text "Choose this name"
  let val = _inputElement_value inputTextBox
      clicked = domEvent Click e

  return $ tag (current val) clicked

signInWidget :: CodewordsM t m => m (Event t ClientMsg)
signInWidget = do
  eventText <- inputTextBoxBtnWidget
  return $ fmap CreateName eventText

homeViewWidget :: CodewordsM t m => User -> m (Event t ClientMsg)
homeViewWidget user = do
  el "div" $ text $ "Welcome " <> name user <> " ID: " <> tShow (userID user)
  e <- inputTextBoxBtnWidget
  return $ fmap (flip CreateRoom Nothing) e

changeView :: ServerMsg -> View -> View
changeView (NameCreated u) _ = LoggedIn u HomeView
changeView (RoomCreated roomID _) (LoggedIn u _) = LoggedIn u $ RoomView roomID
changeView _ v = v

router :: CodewordsM t m => Dynamic t (IntMap Room) -> View -> m (Event t ClientMsg)
router _ SignIn = signInWidget
router _ (LoggedIn u HomeView) = homeViewWidget u
router roomList (LoggedIn u (RoomView n)) = do
  let currentRoom = M.lookup n <$> roomList
  currentRoom' <- maybeDyn currentRoom
  clientMsg <- dyn $ ffor currentRoom' $ \case
    Just r -> displayRoom r >> return never
    Nothing -> text "Room does not exist" >> return never
  switchHold never clientMsg

tShow :: Show a => a -> T.Text
tShow = T.pack.show

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Codewords"
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

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
      prerender_ blank $ mdo
        serverMsg <- codeWordsSocket clientMsg

        currentView <- foldDyn changeView SignIn serverMsg
        newServerMsg <- dyn $ router roomList <$> currentView
        clientMsg <- switchHold never newServerMsg

        roomList <- roomsWidget $ fmapMaybe (preview _RoomCreated) serverMsg
        simpleList (fmap elems roomList) displayRoomCard
        --showMsgs name
        return ()
        --randomBoard <- liftIO $ newGame ["Bob", "your uncle", "blah", "turd", "123124", "zcdsa"]
        --text $ T.pack.show $ randomBoard

      return ()
  }
