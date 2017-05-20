{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RealWorld.Monad
  ( RealWorld, runRealWorld ) where

import RealWorld.Prelude

import Control.Monad.Time (MonadTime(..))
import Data.Acid          (AcidState)
import Data.Time          (getCurrentTime)

import qualified RealWorld.DB           as DB
import           RealWorld.Model.Errors (Errors)

newtype RealWorld a
  = RealWorld
      { unrealWorld
          :: ExceptT Errors
               (ReaderT (AcidState DB.Database) IO)
               a
      }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadThrow, MonadError Errors
    , MonadReader (AcidState DB.Database)
    )

instance Alternative RealWorld where
  empty = RealWorld $ throwError mempty
  RealWorld a <|> RealWorld b = RealWorld (a <|> b)

instance MonadPlus RealWorld where
  mzero = empty
  mplus = (<|>)

instance MonadTime RealWorld where
  currentTime = liftIO getCurrentTime

instance DB.MonadAcid DB.Database RealWorld where
  getAcidState = ask

runRealWorld
  :: AcidState DB.Database
  -> RealWorld a
  -> ExceptT Errors IO a
runRealWorld db =
  mapExceptT (`runReaderT` db) . unrealWorld
