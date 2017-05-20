{-# LANGUAGE OverloadedStrings #-}
module RealWorld.Api.Articles where

import RealWorld.Api.Prelude

import Control.Monad.Time (currentTime)

import qualified RealWorld.DB            as DB
import           RealWorld.Model.Article
  (Article(..), ArticleBody(..), ArticlesBody(..), Slug)
import qualified RealWorld.Model.Article as Article
import qualified RealWorld.Model.Errors  as Errors
import qualified RealWorld.Model.Field   as Field

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
  title_ <- Field.require a title
  desc_  <- Field.require a description
  body_  <- Field.require a body
  tags_  <- Field.require a tagList
              <|> pure mempty
  slug_  <- Article.sluggify title_
              ?? Errors.unprocessable "Unsluggable title"
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
  Errors.notFound "Requested article doesn't exist."
