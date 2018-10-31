{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module LocalStorage where
import Reflex.Dom
import Control.Monad.IO.Class
import Control.Monad (void)
import Data.Text (Text)
import Language.Javascript.JSaddle
import Control.Lens ((^.))

saveLocal :: Text -> JSM ()
saveLocal msg = do
  jsg "window"
    ^. js "localStorage"
    ^. jss "saveLocation" [msg]
  return ()

getLocal :: JSM (Maybe Text)
getLocal = do
  jsv <- jsg "window"
    ^. js "localStorage"
    ^. js "saveLocation"
  liftJSM (fromJSVal jsv)

saveKV :: Text -> Text -> JSM ()
saveKV k v = do
  jsg "window"
    ^. js "localStorage"
    ^. jss "setItem" [k, v]
  return ()

getKV :: Text -> JSM (Maybe Text)
getKV k = do
  jsv <- (jsg "window"
    ^. js "localStorage"
    ^. js1 "getItem" [k])
  liftJSM (fromJSVal jsv)

localStorageTest :: MonadWidget t m => m ()
localStorageTest = do
  ctx <- unJSContextSingleton <$> askJSContext
  el "div" $ do
    t <- textInput def
    set <- button "set value"
    performEvent_ $ ffor (tag (current $ value t) set) $ liftIO . runJSaddle ctx . saveLocal
  el "div" $ do
    get <- button "get value"
    val <- performEvent $ ffor get $ \_ -> liftIO . runJSaddle ctx $ getLocal
    display =<< holdDyn Nothing val
  return ()
