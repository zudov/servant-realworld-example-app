module RealWorld.Api.Profiles where

import RealWorld.Prelude

import Servant

import RealWorld.Monad

type Api =
  Get '[JSON] ()

server :: ServerT Api RealWorld
server =
  pure ()
