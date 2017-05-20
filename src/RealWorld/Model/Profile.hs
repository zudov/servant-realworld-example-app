{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module RealWorld.Model.Profile
  ( Profile(..) ) where

import RealWorld.Prelude

import           Data.Aeson    ((.=))
import qualified Data.Aeson    as Json
import           Data.SafeCopy (base, deriveSafeCopy)

import           RealWorld.Model.Field (Field)
import qualified RealWorld.Model.Field as Field
import           RealWorld.Model.Url   (Url)

data Profile = Profile
  { username  :: Field "username"  Text
  , bio       :: Field "bio"       Text
  , image     :: Field "image"     Url
  , following :: Field "following" Bool
  } deriving (Show, Eq, Ord)

$(deriveSafeCopy 0 'base ''Profile)

instance FromJSON Profile where
  parseJSON json = do
    o <- parseJSON json
    username  <- Field.objectKey @"username" o
    bio       <- Field.objectKey @"bio" o
    image     <- Field.objectKey @"image" o
    following <- pure Field.Undefined
    pure Profile{..}

instance ToJSON Profile where
  toJSON Profile{..} =
    Json.object
      [ "username"  .= username
      , "bio"       .= bio
      , "image"     .= image
      , "following" .= following
      ]
