module DevelMain where


import RealWorld.Prelude

import Data.Acid    (AcidState, openLocalState)
import RealWorld.DB (Database, initialDatabase)

import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai

import qualified Main
import qualified Rapid

update :: IO ()
update =
  Rapid.rapid 0 $ \r -> do
    Rapid.restart r "webserver" $ do
      putStrLn "(re)starting the webserver"
      acid <- Rapid.createRef r "acid"
                $ openLocalState initialDatabase
      webserver acid

stop :: IO ()
stop =
  Rapid.rapid 0 $ \r -> do
    Rapid.stop r "webserver" webserver

webserver :: AcidState Database -> IO ()
webserver acid =
  Warp.run 8080
    $ Wai.logStdoutDev
    $ Main.app acid
