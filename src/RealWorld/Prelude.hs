module RealWorld.Prelude
  ( module A
  , failWith
  , (??)
  , failWithM
  , (!?)
  ) where

import Prelude as A

import Control.Applicative    as A (Alternative(..), optional)
import Control.Monad          as A (MonadPlus(..), guard, mfilter, when)
import Control.Monad.Catch    as A (MonadThrow(..), catch)
import Control.Monad.Except   as A
  (ExceptT(..), MonadError(..), mapExceptT, runExceptT, withExceptT)
import Control.Monad.IO.Class as A (MonadIO, liftIO)
import Control.Monad.Reader   as A
  (MonadReader(..), ReaderT(..), ask, asks, runReaderT)
import Control.Monad.State    as A
  (MonadState(..), get, gets, modify, modify, put)
import Control.Monad.Time     as A (MonadTime(..))
import Data.Aeson             as A (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Foldable          as A (fold)
import Data.Function          as A ((&))
import Data.Hashable          as A (Hashable(..))
import Data.HashMap.Lazy      as A (HashMap)
import Data.Map               as A (Map)
import Data.Maybe             as A (isJust)
import Data.Monoid            as A ((<>))
import Data.Proxy             as A (Proxy(Proxy))
import Data.Set               as A (Set)
import Data.Text              as A (Text)
import Data.Time              as A (UTCTime)
import Data.Traversable       as A (for)
import Data.Vector            as A (Vector)
import Data.Void              as A (Void)
import GHC.Generics           as A (Generic)
import GHC.TypeLits           as A (KnownSymbol, Symbol, symbolVal)
import Numeric.Natural        as A (Natural)
import Web.HttpApiData        as A (FromHttpApiData(..), ToHttpApiData(..))

failWith :: MonadError e m => e -> Maybe a -> m a
failWith e = maybe (throwError e) pure

(??) :: MonadError e m => Maybe a -> e -> m a
(??) = flip failWith

failWithM :: MonadError e m => e -> m (Maybe a) -> m a
failWithM e m = m >>= failWith e

(!?) :: MonadError e m => m (Maybe a) -> e -> m a
(!?) = flip failWithM
