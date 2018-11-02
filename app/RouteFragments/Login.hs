{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module RouteFragments.Login(loginPage) where

import           Control.Applicative       ((<$>))
import           Data.Aeson
import qualified Data.Text                 as T
import           Control.Lens
import           GHC.Generics
import           Reflex.Dom.Core
import           Reflex.Dom.Routing.Writer
import           Reflex.Dom.Storage.Class
import           Types.RouteFragment
import           UI.Base                   (WebUiM)
import           UI.LocalStorage
import           GHCJS.DOM.Document
import           GHCJS.DOM
import           Data.Maybe
import           GHCJS.DOM.Types               (MonadJSM)
import Language.Javascript.JSaddle


loginPage :: forall t m. (WebUiM t m) => m ()
loginPage = mdo
    ctx <- unJSContextSingleton <$> askJSContext
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
    tellStorageInsert AuthTag sResult
    tellRouteAs [RouteFragment "settings"] sResult
    tellRouteAs [RouteFragment "settings"] =<< button "Reset"
    evReset <- button "Reset"
    performEvent_ $ fmap (\_ -> runJSaddle ctx (setLan "en")) evReset
    v <- performEvent $ ffor evReset $ \_ -> runJSaddle ctx getLan
    dynText =<< (holdDyn "default" v)
    blank

usernameInput :: MonadWidget t m => (Event t ()) -> m (Dynamic t T.Text)
usernameInput r = do
    el "h4" $ text "Email"
    t <- textInput $ def & setValue .~ ("" <$ r)
    return $ t ^. textInput_value

passwordInput :: MonadWidget t m => (Event t ()) -> m (Dynamic t T.Text)
passwordInput r = do
    el "h4" $ text "Password"
    t <- textInput $ def & textInputConfig_inputType .~ "password"  & setValue .~ ("" <$ r)
    return $ t ^. textInput_value

buildReq :: ToJSON a => a -> XhrRequest T.Text
buildReq t = (postJson "https://staging.chat.chat/login?persist=true" t) & xhrRequest_config.xhrRequestConfig_headers .~ ("content-type" =: "application/json")

data LoginReq = LoginReq {email :: !T.Text, password :: !T.Text} deriving (Eq, Show, Generic, ToJSON)
data FailRes = FailRes {code:: !Int, message :: !T.Text, label :: !T.Text} deriving (Eq, Show, Generic, FromJSON)


setLan :: (MonadJSM m) => String -> m ()
setLan lang = do
    d <- currentDocument
    fromJust $ setCookie <$> d <*> (Just ("lang=" ++ lang))

getLan :: (MonadJSM m) => m T.Text
getLan = do
    d <- currentDocument
    Just (cookie :: String) <- traverse getCookie d
    return $ T.pack cookie
