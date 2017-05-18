module RealWorld.Api.Tags where

import RealWorld.Api.Prelude

type Api =
  Get '[JSON] ()

server :: ServerT Api RealWorld
server =
  pure ()
