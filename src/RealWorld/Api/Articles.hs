{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
module RealWorld.Api.Articles
  ( Api, server ) where

import RealWorld.Prelude

import Servant

import           Control.Monad.Time      (currentTime)
import qualified RealWorld.DB            as DB
import           RealWorld.Model.Article
  (Article(..), ArticleBody(..), ArticlesBody(..), Slug)
import qualified RealWorld.Model.Article as Article
import           RealWorld.Model.Field   (Field)
import qualified RealWorld.Model.Field   as Field
import           RealWorld.Monad

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
  pure $ ArticlesBody articles

createArticle :: ArticleBody -> RealWorld ArticleBody
createArticle (ArticleBody a) = ArticleBody <$> do
  title_ <- require a "title"       title
  desc_  <- require a "description" description
  body_  <- require a "body"        body
  tags_  <- require a "tagList"     tagList
              <|> pure mempty
  slug_  <- Article.sluggify title_
              ?? unprocessable "Unsluggable title"
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
  article <$ DB.update (DB.UpsertArticle slug_ article)

require
  :: (MonadError Errors m, Alternative m)
  => a -> Text -> (a -> Field b) -> m b
require a n f =
  Field.toValue (f a)
    <|> throwError
          (unprocessable $ "Field " <> toUrlPiece n <> " is required")

getArticle :: Slug -> RealWorld ArticleBody
getArticle slug = ArticleBody <$> do
  DB.query (DB.GetArticle slug)
    !? articleNotFound

updateArticle :: Slug -> ArticleBody -> RealWorld ArticleBody
updateArticle slug (ArticleBody article') = ArticleBody <$> do
  ArticleBody article <- getArticle slug
  let article'' = article
        { title       = title       article' <|> title       article
        , description = description article' <|> description article
        , body        = body        article' <|> body        article
        , tagList     = tagList     article' <|> tagList     article
        }
  DB.update (DB.UpsertArticle slug article'')
  pure article''

deleteArticle :: Slug -> RealWorld ()
deleteArticle slug = do
  _article <- DB.update (DB.DeleteArticle slug)
                !? articleNotFound
  pure ()

articleNotFound :: Errors
articleNotFound =
  notFound "Requested article doesn't exist."
