module DevelMain where

import RealWorld.Prelude

import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai

import qualified Main
import qualified Rapid

update :: IO ()
update =
  Rapid.rapid 0 $ \r -> do
    Rapid.restart r "webserver" $ do
      putStrLn "(re)starting the webserver"
      webserver

stop :: IO ()
stop =
  Rapid.rapid 0 $ \r -> do
    Rapid.stop r "webserver" webserver

webserver :: IO ()
webserver =
  Warp.run 8080
    $ Wai.logStdoutDev Main.app
