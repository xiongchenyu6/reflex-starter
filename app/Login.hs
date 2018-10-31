{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Login(loginPage) where

import           Data.Aeson
import           Data.Aeson.TH
import           Control.Applicative              ((<$>), (<*>))
import           Control.Monad                    (void, (<=<))
import qualified Data.ByteString                  as BS
import           Data.FileEmbed
import qualified Data.Map                         as M
import           Data.Maybe                       (fromMaybe, listToMaybe)
import qualified Data.Text                        as T (Text, isPrefixOf, pack,
                                                        unpack)
import           GHCJS.DOM.EventM                 (on)
import           GHCJS.DOM.FileReader             (getResult, load,
                                                   newFileReader, readAsDataURL)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Types (MonadDOM, fromJSString, toJSString, ToJSString)
import           GHC.Generics
import           Icon                             (chatchatIcon)
import           Language.Javascript.JSaddle
import qualified Language.Javascript.JSaddle.Warp as JSW
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.Xhr
import           Reflex.Dom.Routing.Nested
import           Reflex.Dom.Routing.Writer
import           Text.Read                        (readMaybe)
import           Token
import Reflex.Dom.Storage.Base
import Reflex.Dom.Storage.Class

loginPage :: (MonadWidget t m, HasStorage t ExampleTag m, RouteWriter t segment m, HasRoute t segment m) => m ()
loginPage = do
  rec
    user <- usernameInput evReset
    pswd <- passwordInput evReset
    let values = zipDynWith (LoginReq) user pswd
    b <- button "login"
    let c = buildReq <$> tagPromptlyDyn values b
    rep <-  performRequestAsync c
    let sResult = (fmapMaybe (decodeXhrResponse :: XhrResponse -> Maybe SuccessRes) rep)
    let fResult = (fmapMaybe (decodeXhrResponse :: XhrResponse -> Maybe FailRes) rep)
    let l = ffor fResult $ \FailRes{..} -> message
    dynText =<< (holdDyn "default" l)
    tellStorageInsert Tag2 sResult
    evReset <- button "Reset"
  blank

   -- tellRouteAs ["settings"] sResult
usernameInput :: MonadWidget t m => (Event t ()) -> m (Dynamic t T.Text)
usernameInput r = do
    el "h4" $ text "Email"
    t <- textInput $ def & setValue .~ ("" <$ r)
    return $ _textInput_value t

passwordInput :: MonadWidget t m => (Event t ()) -> m (Dynamic t T.Text)
passwordInput r = do
    el "h4" $ text "Password"
    t <- textInput $ def & textInputConfig_inputType .~ "password"  & setValue .~ ("" <$ r)
    return $ _textInput_value t

buildReq :: ToJSON a => a -> XhrRequest T.Text
buildReq t = (postJson "https://staging.chat.chat/login?persist=true" t) & xhrRequest_config.xhrRequestConfig_headers .~ ("content-type" =: "application/json")


data LoginReq = LoginReq {email :: !T.Text, password :: !T.Text} deriving (Eq, Show, Generic, ToJSON)


data FailRes = FailRes {code:: !Int, message :: !T.Text, label :: !T.Text} deriving (Eq, Show, Generic, FromJSON)
