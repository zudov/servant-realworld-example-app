{-# LANGUAGE OverloadedStrings #-}
module RealWorld.Api
  ( Api, server ) where

import Servant

import RealWorld.Monad

type Api = Get '[JSON] Text

server :: ServerT Api RealWorld
server =
  pure "Enter the realworld"
