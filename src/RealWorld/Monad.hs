{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
module RealWorld.Monad
  ( RealWorld, runRealWorld
  , Error, Errors, toServantErr
  , failWith, (??), failWithM, (!?)
  , badRequest, unprocessable, notFound
  ) where

import RealWorld.Prelude

import           Control.Monad.Time        (MonadTime(..))
import           Data.Acid
import           Data.Aeson                ((.=))
import qualified Data.Aeson                as Json
import qualified Data.ByteString.Char8     as BS8
import           Data.Time                 (getCurrentTime)
import qualified Data.Vector               as Vector
import           Network.HTTP.Types.Status
  (Status(..), status400, status404, status422, status500)
import           Servant.Server            (ServantErr(..))

import qualified RealWorld.DB as DB

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

data Error = Error
  { errorStatus  :: Status
  , errorDetails :: Json.Value
  } deriving (Show, Eq)

newtype Errors = Errors (Vector Error)
  deriving (Show, Eq, Monoid)

anError :: Error -> Errors
anError = Errors . Vector.singleton

notFound :: Text -> Errors
notFound = anError . Error status404 . toJSON

badRequest :: Text -> Errors
badRequest = anError . Error status400 . toJSON

unprocessable :: Text -> Errors
unprocessable = anError . Error status422 . toJSON

failWith :: MonadError e m => e -> Maybe a -> m a
failWith e = maybe (throwError e) pure

(??) :: MonadError e m => Maybe a -> e -> m a
(??) = flip failWith

failWithM :: MonadError e m => e -> m (Maybe a) -> m a
failWithM e m = m >>= failWith e

(!?) :: MonadError e m => m (Maybe a) -> e -> m a
(!?) = flip failWithM

toServantErr :: Errors -> ServantErr
toServantErr (Errors errs) =
  ServantErr
    { errHTTPCode     = statusCode status
    , errReasonPhrase = BS8.unpack $ statusMessage status
    , errBody         = Json.encode body
    , errHeaders      = []
    }
  where
    status
      | null errs = status500
      | otherwise = foldl1 add $ fmap errorStatus errs
      where
        add a b =
          if statusCode a > statusCode b then a else b

    body = Json.object
      [ "errors" .= fmap errorDetails errs ]
