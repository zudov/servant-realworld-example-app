{-# LANGUAGE OverloadedStrings #-}
module RealWorld.Api
  ( Api, server ) where

import Servant

import qualified RealWorld.Api.Articles as Articles
import           RealWorld.Monad

type Api =
  "articles" :> Articles.Api

server :: ServerT Api RealWorld
server =
  Articles.server
