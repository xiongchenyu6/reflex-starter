{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import qualified Data.ByteString                  as BS
import qualified Language.Javascript.JSaddle.Warp as JSW
import           Reflex.Dom.Core
import           Data.Maybe            (fromMaybe)
import           Data.FileEmbed
import           Login
import           Language.Javascript.JSaddle
import           Control.Monad         (void)
import           Reflex.Dom.Routing.Nested
import           Reflex.Dom.Routing.Writer
import           Reflex.Dom.Storage.Base
import           Data.GADT.Aeson
import           Reflex.Dom.Storage.Class
import           LocalStorage
import           Token
cssList :: BS.ByteString
cssList = $(embedFile "static/font-awesome.css")
      <> $(embedFile "static/bulma.css")
      <> $(embedFile "static/layout.css")

main :: IO ()
main = JSW.run 3000 $ app

app :: JSM ()
app = mainWidgetWithCss cssList $ route

route :: MonadWidget t m => m ()
route =
  void . runStorageT LocalStorage $ do
    -- sets the default value for Tag1, only if none is already present
    initializeTag Tag1 0
    runRouteWithPathInFragment $ fmap snd $ runRouteWriterT $ do
      void $ withRoute $ \route -> case fromMaybe "" route of
        "" -> do
            loginPage
            tellRouteAs ["settings"] =<< button "Settings"
            localStorageTest
        "settings" -> text "Settings"
        _          -> tellRedirectLocally [""]

mainPage :: MonadWidget t m => m ()
mainPage = undefined
