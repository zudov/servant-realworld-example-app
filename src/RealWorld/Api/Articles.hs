{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module RealWorld.Api.Articles
  ( Api, server ) where

import RealWorld.Prelude

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json

import Servant

import           Control.Monad.Time      (currentTime)
import qualified Data.Vector             as Vector
import qualified RealWorld.DB            as DB
import           RealWorld.Model.Article (Article(..), Slug)
import qualified RealWorld.Model.Article as Article
import           RealWorld.Model.Field   (Field)
import qualified RealWorld.Model.Field   as Field
import           RealWorld.Monad

newtype ArticlesBody = ArticlesBody (Vector Article)

instance ToJSON ArticlesBody where
  toJSON (ArticlesBody articles) =
    Json.object
      [ "articles" .= articles ]

newtype ArticleBody = ArticleBody Article
  deriving (Show, Eq, Ord)

instance ToJSON ArticleBody where
  toJSON (ArticleBody article) =
    Json.object
      [ "article" .= article ]

instance FromJSON ArticleBody where
  parseJSON json = do
    o <- parseJSON json
    article <- o .: "article"
    pure $ ArticleBody article

type Api =
  GetArticles
    :<|>
  GetArticle
    :<|>
  CreateArticle
    :<|>
  UpdateArticle
    :<|>
  DeleteArticle

type GetArticles =
  Get '[JSON] ArticlesBody

type GetArticle =
  Capture "slug" Slug
    :> Get '[JSON] ArticleBody

type CreateArticle =
  ReqBody '[JSON] ArticleBody
    :> Post '[JSON] ArticleBody

type UpdateArticle =
  Capture "slug" Slug
    :> ReqBody '[JSON] ArticleBody
       :> Put '[JSON] ArticleBody

type DeleteArticle =
  Capture "slug" Slug
    :> Delete '[JSON] ()

server :: ServerT Api RealWorld
server =
  getArticles
    :<|>
  getArticle
    :<|>
  createArticle
    :<|>
  updateArticle
    :<|>
  deleteArticle

getArticles :: RealWorld ArticlesBody
getArticles = do
  articles <- DB.query DB.GetArticles
  pure $ ArticlesBody $ Vector.fromList articles

createArticle :: ArticleBody -> RealWorld ArticleBody
createArticle (ArticleBody a) = do
  -- required fields
  title_ <- a & getField "title"       title
  desc_  <- a & getField "description" description
  body_  <- a & getField "body"        body
  -- optional fields
  let tags_ = a & getField "tagList"     tagList
                & fold @(Either RealWorldErr)
  -- derived and generated fields
  slug_ <- Article.sluggify title_
         & maybe (throwError $ unprocessable
                   [ "Unsluggable title: " <> show title_])
             pure
  createdAt_ <- currentTime

  let article = Article
        { slug           = pure slug_
        , title          = pure title_
        , description    = pure desc_
        , body           = pure body_
        , tagList        = pure tags_
        , createdAt      = pure createdAt_
        , updatedAt      = Field.Nil
        , favorited      = Field.Undefined
        , favoritesCount = pure 0
        , author         = Field.Undefined -- FIXME: DEFINE
        }
  _ <- DB.update $ DB.UpsertArticle slug_ article
  pure $ ArticleBody article

getField
  :: (MonadError RealWorldErr m)
  => Text -> (a -> Field b) -> a -> m b
getField t f a =
  case f a of
    Field.Value v ->
      pure v
    Field.Undefined ->
      throwError
        $ unprocessable
            [ "Field " <> show t <> " is undefined" ]
    Field.Nil ->
      throwError
        $ unprocessable
            [ "Field " <> show t <> " is null" ]


getArticle :: Slug -> RealWorld ArticleBody
getArticle slug =
  throwError
    $ notFound [ "No article with slug " <> show slug <> " found" ]

updateArticle :: Slug -> ArticleBody -> RealWorld ArticleBody
updateArticle slug (ArticleBody article') = do
  mArticle <- DB.query $ DB.GetArticle slug
  case mArticle of
    Nothing ->
      throwError
        $ notFound [ "No article with slug " <> show slug <> " found" ]
    Just article -> do
      let article'' = article
            { title       = title       article' <|> title       article
            , description = description article' <|> description article
            , body        = body        article' <|> body        article
            , tagList     = tagList     article' <|> tagList     article
            }
      DB.update $ DB.UpsertArticle slug article''
      pure $ ArticleBody article''

deleteArticle :: Slug -> RealWorld ()
deleteArticle slug = do
  mArticle <- DB.update $ DB.DeleteArticle slug
  case mArticle of
    Nothing ->
      throwError
        $ notFound [ "No article with slug " <> show slug <> " found" ]
    Just _ ->
      pure ()

