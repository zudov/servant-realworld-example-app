{-# LANGUAGE TemplateHaskell #-}
module RealWorld.DB where

import RealWorld.Prelude

import           Data.Acid     as Acid
import qualified Data.Map      as Map
import           Data.SafeCopy

import RealWorld.Model.Article (Article(), Slug)

data Database = Database
  { dbArticles :: Map Slug Article }

class (Monad m, MonadIO m) => MonadAcid state m | m -> state where
  getAcidState :: m (AcidState state)
  update :: (UpdateEvent event, EventState event ~ state)
         => event -> m (EventResult event)
  update e = do
    acid <- getAcidState
    liftIO $ Acid.update acid e
  query :: (QueryEvent event, EventState event ~ state)
        => event -> m (EventResult event)
  query e = do
    acid <- getAcidState
    liftIO $ Acid.query acid e

initialDatabase :: Database
initialDatabase = Database
  { dbArticles = mempty }

$(deriveSafeCopy 0 'base ''Database)

getArticle :: Slug -> Query Database (Maybe Article)
getArticle slug = do
  Map.lookup slug <$> asks dbArticles

getArticles :: Query Database [Article]
getArticles =
  Map.elems <$> asks dbArticles

upsertArticle :: Slug -> Article -> Update Database ()
upsertArticle slug article = do
  db <- get
  put $ db { dbArticles = Map.insert slug article $ dbArticles db }

deleteArticle :: Slug -> Update Database (Maybe Article)
deleteArticle slug = do
  article <- gets (Map.lookup slug . dbArticles)
  when (isJust article) $ modify $ \db ->
    db { dbArticles = Map.delete slug $ dbArticles db }
  pure article

$(makeAcidic ''Database ['getArticle, 'getArticles, 'upsertArticle, 'deleteArticle])
