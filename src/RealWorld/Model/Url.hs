{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module RealWorld.Model.Url
  ( Url(..) ) where

import RealWorld.Prelude

import Data.SafeCopy (base, deriveSafeCopy)

newtype Url = Url { runUrl :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

$(deriveSafeCopy 0 'base ''Url)
