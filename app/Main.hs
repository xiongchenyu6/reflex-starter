{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Monad                    (void)
import qualified Data.ByteString                  as BS
import           Data.FileEmbed
import           Data.Maybe                       (fromMaybe)
import           Language.Javascript.JSaddle
import qualified Language.Javascript.JSaddle.Warp as JSW
import           Reflex.Dom.Core
import           Reflex.Dom.Routing.Nested
import           Reflex.Dom.Routing.Writer
import           RouteFragments.Login
import           Types.RouteFragment
import           UI.Base                          (WebUiM, runUiT)

cssList :: BS.ByteString
cssList = $(embedFile "static/font-awesome.css")
      <> $(embedFile "static/bulma.css")
      <> $(embedFile "static/layout.css")

main :: IO ()
main = JSW.run 3000 $ app

app :: JSM ()
app = mainWidgetWithCss cssList $ mkUi

mkUi :: forall t m. MonadWidget t m => m ()
mkUi = runUiT $ void mkWeb

mkWeb :: forall t m. (WebUiM t m) => m ()
mkWeb =
  void $ withRoute $ \r -> case fromMaybe (RouteFragment "") r of
    RouteFragment ""         -> loginPage
    RouteFragment "settings" -> text "Settings"
    RouteFragment _          -> tellRedirectLocally [RouteFragment ""]
