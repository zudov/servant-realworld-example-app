{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module RealWorld.Model.Errors where

import RealWorld.Prelude

import           Data.Aeson                ((.=))
import qualified Data.Aeson                as Json
import qualified Data.ByteString.Char8     as BS8
import qualified Data.Vector               as Vector
import           Network.HTTP.Types.Status
  (Status(..), status400, status404, status422, status500)
import           Servant.Server            (ServantErr(..))


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
