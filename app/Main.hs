{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import Reflex.Dom.Core
import Data.Map
import Data.Text (pack, unpack, Text)
import Text.Read (readMaybe)
import Control.Applicative ((<*>), (<$>))
import qualified Language.Javascript.JSaddle.Warp as JSW

main :: IO ()
main = JSW.run 3000 $ mainWidget $ el "div" $ do
  nx <- numberInput
  d <- dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
      resultText = fmap (pack . show) result
  text " = "
  dynText resultText

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let errorState = "style" =: "border-color: red"
      validState = "style" =: "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
                           & textInputConfig_attributes .~ attrs
      let result = fmap (readMaybe . unpack) $ _textInput_value n
          attrs  = fmap (maybe errorState (const validState)) result
  return result

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

ops :: Map Op Text

ops = fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
            Plus -> (+)
            Minus -> (-)
            Times -> (*)
            Divide -> (/)
