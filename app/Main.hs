module Main where

import Prelude

import Data.Proxy (Proxy(..))

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Servant                              as Servant

import RealWorld.Api (Api, server)

main :: IO ()
main =
  Warp.run 8080
    $ Wai.logStdout app

app :: Wai.Application
app = Servant.serve @Api Proxy server
