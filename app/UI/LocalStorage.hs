{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase     #-}

module UI.LocalStorage where

import           Data.Aeson
import           Data.Dependent.Map    (Some (..))
import           Data.Dependent.Sum    (ShowTag (..))
import           Data.Functor.Identity (Identity (..))
import           Data.GADT.Aeson
import           Data.GADT.Compare
import           Data.GADT.Show
import           Data.Text             as T
import           GHC.Generics

data SuccessRes = SuccessRes {expires_in :: !Int, access_token :: !T.Text, user :: !T.Text,  token_type :: !T.Text} deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data AppTag a where
  AuthTag :: AppTag SuccessRes

instance GEq AppTag where
  geq AuthTag AuthTag = Just Refl
  geq _ _             = Nothing

instance GCompare AppTag where
  gcompare AuthTag AuthTag = GEQ
  gcompare AuthTag _       = GLT
  gcompare _ AuthTag       = GGT

instance GShow AppTag where
  gshowsPrec _p AuthTag = showString "AuthTag"

instance ShowTag AppTag Identity where
  showTaggedPrec AuthTag = showsPrec

instance GKey AppTag where
  toKey (This AuthTag) = "authTag"

  fromKey = \case
      "tag1" -> Just (This AuthTag)
      _      -> Nothing

  keys _ = [This AuthTag]

instance ToJSONTag AppTag Identity where
  toJSONTagged AuthTag (Identity x) = toJSON x

instance FromJSONTag AppTag Identity where
  parseJSONTagged AuthTag x = Identity <$> parseJSON x
