{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module RealWorld.Model.User where

import RealWorld.Prelude

import Data.Aeson       ((.=))
import Data.Aeson       as Json
import Data.Aeson.Types as Json

import RealWorld.Model.Profile

newtype Token = Token Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

data UserBody
  = UserBody        User
  | UserProfileBody User Profile
  deriving (Show, Eq, Ord)

instance ToJSON UserBody where
  toJSON (UserBody User{..}) = Json.object
    [ "email" .= email
    , "token" .= token
    ]
  toJSON (UserProfileBody user profile) =
    mergeObjects [toJSON (UserBody user), toJSON profile]
    where
      mergeObjects =
        Json.Object . foldMap (fold . Json.parse parseJSON)


data User = User
  { email :: Text
  , token :: Token
  } deriving (Show, Eq, Ord)
