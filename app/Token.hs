{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Token where

import           Data.Aeson
import           GHC.Generics
import           Data.Text        as T
import Data.Functor.Identity (Identity(..))
import Data.Dependent.Map (Some(..))
import Data.Dependent.Sum (ShowTag(..))
import Data.GADT.Show
import Data.GADT.Compare
import Data.GADT.Aeson

data SuccessRes = SuccessRes {expires_in :: !Int, access_token :: !T.Text, user :: !T.Text,  token_type :: !T.Text} deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ExampleTag a where
  Tag1 :: ExampleTag Int
  Tag2 :: ExampleTag SuccessRes

instance GEq ExampleTag where
  geq Tag1 Tag1 = Just Refl
  geq Tag2 Tag2 = Just Refl
  geq _ _       = Nothing

instance GCompare ExampleTag where
  gcompare Tag1 Tag1 = GEQ
  gcompare Tag1 _    = GLT
  gcompare _ Tag1    = GGT
  gcompare Tag2 Tag2 = GEQ

instance GShow ExampleTag where
  gshowsPrec _p Tag1 = showString "Tag1"
  gshowsPrec _p Tag2 = showString "Tag2"

instance ShowTag ExampleTag Identity where
  showTaggedPrec Tag1 = showsPrec
  showTaggedPrec Tag2 = showsPrec

instance GKey ExampleTag where
  toKey (This Tag1) = "tag1"
  toKey (This Tag2) = "tag2"

  fromKey t =
    case t of
      "tag1" -> Just (This Tag1)
      "tag2" -> Just (This Tag2)
      _      -> Nothing

  keys _ = [This Tag1, This Tag2]

instance ToJSONTag ExampleTag Identity where
  toJSONTagged Tag1 (Identity x) = toJSON x
  toJSONTagged Tag2 (Identity x) = toJSON x

instance FromJSONTag ExampleTag Identity where
  parseJSONTagged Tag1 x = Identity <$> parseJSON x
  parseJSONTagged Tag2 x = Identity <$> parseJSON x
