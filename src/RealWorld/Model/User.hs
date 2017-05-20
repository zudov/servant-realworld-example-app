{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module RealWorld.Model.User where

import RealWorld.Prelude

import           Data.Aeson       ((.:), (.=))
import qualified Data.Aeson       as Json
import qualified Data.Aeson.Types as Json

import RealWorld.Model.Field   (Field)
import RealWorld.Model.Field   as Field
import RealWorld.Model.Profile

newtype Token = Token Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data UserProfileBody = UserProfileBody User Profile
  deriving (Show, Eq, Ord)

instance ToJSON UserProfileBody where
  toJSON (UserProfileBody user profile) =
    mergeObjects [toJSON user, toJSON profile]
    where
      mergeObjects =
        Json.Object . foldMap (fold . Json.parse parseJSON)

instance FromJSON UserProfileBody where
  parseJSON json =
    UserProfileBody
      <$> parseJSON json
      <*> parseJSON json

newtype UserBody = UserBody User
  deriving (Show, Eq, Ord)

instance ToJSON UserBody where
  toJSON (UserBody user) = Json.object
    [ "user" .= user ]

instance FromJSON UserBody where
  parseJSON json = do
    o <- parseJSON json
    user <- o .: "user"
    pure $ UserBody user

data User = User
  { email :: Field "email" Text
  , token :: Field "token" Token
  } deriving (Show, Eq, Ord)

instance ToJSON User where
  toJSON User{..} = Json.object
    [ "email" .= email
    , "token" .= token
    ]

instance FromJSON User where
  parseJSON json = do
    o <- parseJSON json
    email <- Field.objectKey @"email" o
    token <- Field.objectKey @"token" o
    pure User{..}
