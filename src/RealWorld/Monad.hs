{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module RealWorld.Monad
  ( RealWorld, runRealWorld, RealWorldErr(), toServantErr
  , badRequest, unprocessable, notFound
  ) where

import RealWorld.Prelude

import           Control.Monad.Time (MonadTime(..))
import           Data.Acid
import           Data.Aeson         ((.=))
import qualified Data.Aeson         as Json
import           Data.Time          (getCurrentTime)
import           Servant.Server     ((:~>)(Nat), ServantErr(..), err400, err404)

import qualified RealWorld.DB as DB

newtype RealWorld a
  = RealWorld
      { unrealWorld
          :: ReaderT (AcidState DB.Database)
               (ExceptT RealWorldErr IO)
               a
      }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadThrow, MonadError RealWorldErr
    , MonadReader (AcidState DB.Database)
    )

instance MonadTime RealWorld where
  currentTime = liftIO getCurrentTime

instance DB.MonadAcid DB.Database RealWorld where
  getAcidState = ask

runRealWorld
  :: AcidState DB.Database
  -> RealWorld a
  -> ExceptT RealWorldErr IO a
runRealWorld db m = runReaderT (unrealWorld m) db

newtype RealWorldErr
  = RealWorldErr { unRealWorldErr :: ServantErr }

notFound :: ToJSON a => a -> RealWorldErr
notFound details =
  RealWorldErr err404
    { errBody = Json.encode $ Json.object [ "errors" .= details ] }

badRequest :: ToJSON a => a -> RealWorldErr
badRequest details =
  RealWorldErr err400
    { errBody = Json.encode $ Json.object [ "errors" .= details] }

unprocessable :: ToJSON a => a -> RealWorldErr
unprocessable details =
  RealWorldErr $ ServantErr
    { errHTTPCode     = 422
    , errReasonPhrase = "Unprocessable Entity"
    , errBody         = Json.encode $ Json.object [ "errors" .= details]
    , errHeaders      = []
    }


toServantErr :: RealWorldErr -> ServantErr
toServantErr = unRealWorldErr
