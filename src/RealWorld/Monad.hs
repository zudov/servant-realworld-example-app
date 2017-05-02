{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RealWorld.Monad
  ( RealWorld, runRealWorld, RealWorldErr(), toServantErr) where

import Prelude

import Control.Monad.Except   (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Servant.Server         ((:~>)(Nat), ServantErr)

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
