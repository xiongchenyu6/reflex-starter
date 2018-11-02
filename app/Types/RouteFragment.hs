{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types.RouteFragment (
    RouteFragment(..)
  , toSegments
  , fromSegments
  , routeToText
  ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           GHC.Generics
import           Reflex.Dom.Routing.Nested
import           URI.ByteString

data RouteFragment =
  RouteFragment Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON RouteFragment where
instance FromJSON RouteFragment where

toSegments :: URIRef a -> [RouteFragment]
toSegments =
  fmap RouteFragment .
  nullTextToEmptyList .
  Text.splitOn "/" .
  Text.dropAround (== '/') .
  fragAsText
 where
   nullTextToEmptyList [""] = []
   nullTextToEmptyList x    = x

fromSegments :: URIRef a -> [RouteFragment] -> URIRef a
fromSegments u =
  setFrag u . ("/" <>) . routeToText

routeToText :: [RouteFragment]
            -> Text
routeToText =
  Text.intercalate "/" .
  fmap routeFragmentToText

routeFragmentToText :: RouteFragment
                    -> Text
routeFragmentToText (RouteFragment t) =
  t
