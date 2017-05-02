{-# LANGUAGE OverloadedStrings #-}
module RealWorld.Api
  ( Api, server ) where

import Prelude

import Data.Text (Text)
import Servant

type Api = Get '[JSON] Text

server :: Server Api
server =
  pure "Hello realworld"
