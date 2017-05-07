module DevelMain where

import Prelude

import Control.Exception
import Data.Proxy        (Proxy(..))

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai

import qualified Servant

import qualified Main
import qualified Rapid

update :: IO ()
update =
  Rapid.rapid 0 $ \r -> do
    Rapid.start r "webserver" $ do
      putStrLn "(re)starting the webserver"
      webserver

webserver :: IO ()
webserver =
  Warp.run 8080
    $ Wai.logStdoutDev Main.app
