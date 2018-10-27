{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative              ((<$>), (<*>))
import qualified Data.ByteString                  as BS
import qualified           Data.Map               as M
import qualified Data.Text                        as T (isPrefixOf, Text, pack, unpack)
import           GHCJS.DOM.Types                  (File (..))
import qualified Language.Javascript.JSaddle.Warp as JSW
import           Reflex.Dom.Core
import           Text.Read                        (readMaybe)
import           Data.Maybe            (listToMaybe, fromMaybe)
import           GHCJS.DOM.EventM      (on)
import           GHCJS.DOM.FileReader  (newFileReader, readAsDataURL, load
                                       , getResult)
import           Data.FileEmbed
import           GHCJS.DOM.Types       (File (..))
import           Language.Javascript.JSaddle
import           Control.Monad         ((<=<))
import           Icon                  (chatchatIcon)

-- htmlHead :: MonadWidget t m => m ()
-- htmlHead = do
--   elAttr "meta" ("name" =: "viewpoint" <> "content" =: "width=device-width, initial-scale=1") (return ())
--   styleSheet "https://unpkg.com/purecss@1.0.0/build/pure-min.css"
--   styleSheet $(embedFile "static/layout.css")
--   where
--     styleSheet addr = elAttr "link" ("rel"  =: "stylesheet" <> "href" =: addr) (return ())

cssList :: BS.ByteString
cssList = $(embedFile "static/font-awesome.css")
      <> $(embedFile "static/pure.css")
      <> $(embedFile "static/layout.css")

header :: MonadWidget t m => m ()
header =
  el "div" $ do
    el "strong" $ do
      linkNewTab "https://github.com/reflex-frp/reflex-dom" "Reflex.Dom"
      text " FileInput test page"
    el "p" $ text "Select an image file."

main :: IO ()
main = JSW.run 3000 $ app

app :: JSM ()
app = mainWidgetWithCss cssList $ el "div" $ do
  bodyElement1
  _ <- header
  _ <- chatchatIcon
  display =<< count =<< button "ClickMe"
  bodyElement
  body
  rec
    dynBool <- toggle False evClick
    let dynAttrs = attrs <$> dynBool
    elDynAttr "h1" dynAttrs $ text "Changing color"
    evClick <- button "Change Color"
  nx <- numberInput
  d <- dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
      resultText = fmap (T.pack . show) result
  text " = "
  dynText resultText
  filesDyn <- value <$> fileInput def
  urlE <- fmap (ffilter ("data:image" `T.isPrefixOf`))
      . dataURLFileReader
      . fmapMaybe listToMaybe
      . updated $ filesDyn
  _ <-el "div"
      . widgetHold blank
      . ffor urlE $ \url ->
          elAttr "img" ("src" =: url <> "style" =: "max-width: 80%") blank
  footer

body :: MonadWidget t m => m ()
body  = el "div" $ do
  el "h2" $ text "Swiss Meteo Data (raw version)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  -- Build and send the request
  evStart <- getPostBuild
  let evCode = tagPromptlyDyn (value dd) $ leftmost [ () <$ _dropdown_change dd, evStart]
  evRsp <- performRequestAsync $ buildReq <$> evCode
  -- Display the whole response
  el "h5" $ text "Response Text:"
  let evResult = (fromMaybe "" . _xhrResponse_responseText) <$> evRsp
  dynText =<< holdDyn "" evResult
  return ()

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" ("https://opendata.netcetera.com/smn/smn/" <> code) def

stations :: M.Map T.Text T.Text
stations = M.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]

bodyElement1 :: MonadWidget t m => m ()
bodyElement1 = el "div" $ do
  el "h2" $ text "Button enabled / disabled"
  cb <- el "label" $ do
    cb1 <- checkbox True def
    text "Enable or Disable the button"
    return cb1
  el "p" blank
  counter :: Dynamic t Int <- count =<< disaButton (value cb) "Click me"
  el "p" blank
  display counter

-- | A button that can be enabled and disabled
disaButton :: MonadWidget t m
            => Dynamic t Bool -- ^ enable or disable button
            -> T.Text         -- ^ Label
            -> m (Event t ())
disaButton enabled label = do
    let attrs = ffor enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    b <- elDynAttr "button" attrs $ text label
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

-- | A little helper function for data types in the *Monoid* type class:
-- If the boolean is True, return the first parameter, else return the null or empty element of the monoid
monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty

bodyElement :: MonadWidget t m => m ()
bodyElement = do
    el "h2" $ text "Using foldDyn with function application"
    rec
       dynNum <- foldDyn ($) (0 :: Int) $ leftmost [(+ 1) <$ evIncr, (+ (-1)) <$ evDecr, const 0 <$ evReset]
       el "div" $ display dynNum
       evIncr <- button "Increment"
       evDecr <- button "Decrement"
       evReset <- button "Reset"
    el "h2" $ text "Text Input - Configuration"

    el "h4" $ text "Max Length 14"
    t1 <- textInput $ def & attributes .~ constDyn ("maxlength" =: "14")
    dynText $ _textInput_value t1

    el "h4" $ text "Initial Value"
    t2 <- textInput $ def & textInputConfig_initialValue .~ "input"
    dynText $ _textInput_value t2

    el "h4" $ text "Input Hint"
    t3 <- textInput $
          def & attributes .~ constDyn("placeholder" =: "type something")
    dynText $ _textInput_value t3

    el "h4" $ text "Password"
    t4 <- textInput $ def & textInputConfig_inputType .~ "password"
    dynText $ _textInput_value t4

    el "h4" $ text "Multiple Attributes: Hint + Max Length"
    t5 <- textInput $  def & attributes .~ constDyn ("placeholder" =: "Max 6 chars" <> "maxlength" =: "6")
    dynText $ _textInput_value t5

    el "h4" $ text "Numeric Field with initial value"
    t6 <- textInput $ def & textInputConfig_inputType .~ "number"
                          & textInputConfig_initialValue .~ "0"
    dynText $ _textInput_value t6
    el "h2" $ text "Text Input - Read Value on Button Click"

    ti <- textInput def
    evClick <- button "Click Me"
    el "br" blank
    text "Contents of TextInput on last click: "
    let evText = tagPromptlyDyn (value ti) evClick

    el "h1" $ text "Write into TextInput Widget"
    t1 <- textInput def
    evCopy <- button ">>>"
    let evText = tagPromptlyDyn (value t1) evCopy
    t2 <- textInput $ def & setValue .~ evText
    return ()
    dynText =<< holdDyn "" evText
    el "h2" $ text "Range Input"
    rg <- rangeInput $ def & attributes .~ constDyn
        ("min" =: "-100" <> "max" =: "100" <> "value" =: "0" <> "step" =: "10" <> "list" =: "powers" )
    elAttr "datalist" ("id" =: "powers") $ do
       elAttr "option" ("value" =: "0") blank
       elAttr "option" ("value" =: "-30") blank
       elAttr "option" ("value" =: "50") blank
    el "p" blank
    display $ _rangeInput_value rg
    return ()

attrs :: Bool -> M.Map T.Text T.Text
attrs b = "style" =: ("color: " <> color b)
  where
    color True = "red"
    color _    = "green"

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let errorState = "style" =: "border-color: red"
      validState = "style" =: "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
                           & textInputConfig_attributes .~ attrs
      let result = fmap (readMaybe . T.unpack) $ _textInput_value n
          attrs  = fmap (maybe errorState (const validState)) result
  return result

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

ops :: M.Map Op T.Text

ops = M.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
            Plus   -> (+)
            Minus  -> (-)
            Times  -> (*)
            Divide -> (/)


footer :: MonadWidget t m => m ()
footer = do
  el "hr" $ blank
  el "p" $ do
    text "The code for this example can be found in the "
    linkNewTab "https://github.com/reflex-frp/reflex-examples" "Reflex Examples"
    text " repo."


dataURLFileReader :: (MonadWidget t m) => Event t File -> m (Event t T.Text)
dataURLFileReader request =
  do fileReader <- liftJSM newFileReader
     performEvent_ (fmap (readAsDataURL fileReader .Just) request)
     e <- wrapDomEvent fileReader (`on` load) . liftJSM $ do
       v <- getResult fileReader
       (fromJSVal <=< toJSVal) v
     return (fmapMaybe id e)

linkNewTab :: MonadWidget t m => T.Text -> T.Text -> m ()
linkNewTab href s =
  elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s
