module RealWorld.Api.User where

import RealWorld.Api.Prelude

type Api =
  Get '[JSON] ()

server :: ServerT Api RealWorld
server =
  pure ()
