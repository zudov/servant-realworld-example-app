{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RealWorld.Monad
  ( RealWorld, runRealWorld, RealWorldErr(), toServantErr) where

import RealWorld.Prelude

import           Control.Monad.Time (MonadTime(..))
import           Data.Aeson         ((.=))
import qualified Data.Aeson         as Json
import           Data.Time          (getCurrentTime)
import           Servant.Server     ((:~>)(Nat), ServantErr(..), err400, err404)

newtype RealWorld a
  = RealWorld
      { unrealWorld :: ExceptT RealWorldErr IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runRealWorld :: RealWorld a -> ExceptT RealWorldErr IO a
runRealWorld = unrealWorld

newtype RealWorldErr
  = RealWorldErr { unRealWorldErr :: ServantErr }

toServantErr :: RealWorldErr -> ServantErr
toServantErr = unRealWorldErr
