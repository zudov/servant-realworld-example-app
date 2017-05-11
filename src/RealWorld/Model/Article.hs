{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module RealWorld.Model.Article
  ( Article(..)
  , emptyArticle
  , Slug(..)
  , sluggify
  , Tag(..)
  ) where

import RealWorld.Prelude

import           Data.Aeson    ((.=))
import qualified Data.Aeson    as Json
import qualified Data.Char     as Char
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text     as Text

import           RealWorld.Model.Field   (Field)
import qualified RealWorld.Model.Field   as Field
import           RealWorld.Model.Profile (Profile)

newtype Slug = Slug Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, Hashable)

$(deriveSafeCopy 0 'base ''Slug)

instance FromHttpApiData Slug where
  parseUrlPiece = maybe (Left "This is not a slug") Right . readSlug

sluggify :: Text -> Maybe Slug
sluggify =
  fmap (Slug . Text.intercalate "-")
    . mfilter (not . null)
    . Just . slugWords

readSlug :: Text -> Maybe Slug
readSlug t =
  sluggify t >>= \(Slug s) -> Slug s <$ guard (s == t)

slugWords :: Text -> [Text]
slugWords =
  Text.words . Text.toLower . Text.map mapChar . Text.replace "'" ""
  where
    mapChar a
      | Char.isAlphaNum a = a
      | otherwise = ' '

newtype Tag = Tag Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic)

$(deriveSafeCopy 0 'base ''Tag)

data Article = Article
  { slug           :: Field Slug
  , title          :: Field Text
  , description    :: Field Text
  , body           :: Field Text
  , tagList        :: Field (Vector Tag)
  , createdAt      :: Field UTCTime
  , updatedAt      :: Field UTCTime
  , favorited      :: Field Bool
  , favoritesCount :: Field Int
  , author         :: Field Profile
  } deriving (Show, Eq, Ord)

$(deriveSafeCopy 0 'base ''Article)

emptyArticle :: Article
emptyArticle = Article
  { slug           = Field.Undefined
  , title          = Field.Undefined
  , description    = Field.Undefined
  , body           = Field.Undefined
  , tagList        = Field.Undefined
  , createdAt      = Field.Undefined
  , updatedAt      = Field.Undefined
  , favorited      = Field.Undefined
  , favoritesCount = Field.Undefined
  , author         = Field.Undefined
  }

instance FromJSON Article where
  parseJSON json = do
    o :: Json.Object <- parseJSON json
    slug           <- pure Field.Undefined
    title          <- Field.getJson o "title"
    description    <- Field.getJson o "description"
    body           <- Field.getJson o "body"
    tagList        <- Field.getJson o "tagList"
    createdAt      <- pure Field.Undefined
    updatedAt      <- pure Field.Undefined
    favorited      <- pure Field.Undefined
    favoritesCount <- pure Field.Undefined
    author         <- pure Field.Undefined
    pure Article{..}

instance ToJSON Article where
  toJSON Article{..} =
    Json.object
      [ "slug"           .= slug
      , "title"          .= title
      , "description"    .= description
      , "body"           .= body
      , "tagList"        .= tagList
      , "createdAt"      .= createdAt
      , "updatedAt"      .= updatedAt
      , "favorited"      .= favorited
      , "favoritesCount" .= favoritesCount
      , "author"         .= author
      ]
