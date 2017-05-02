module Main where

import Prelude

import Control.Monad.Except (ExceptT, mapExceptT, withExceptT)
import Data.Proxy           (Proxy(..))

import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import           Servant                              ((:~>), ServantErr)
import qualified Servant                              as Servant

import RealWorld.Api   (Api, server)
import RealWorld.Monad (RealWorld, RealWorldErr(), runRealWorld, toServantErr)

main :: IO ()
main =
  Warp.run 8080
    $ Wai.logStdout app

app :: Wai.Application
app =
  Servant.serve @Api Proxy
    $ Servant.enter enterNat server
  where
    enterNat :: RealWorld :~> Servant.Handler
    enterNat =
      Servant.Nat
        -- NOTE: You can use `mapExceptT` if you wanna map all the way
        $ withExceptT toServantErr
        . runRealWorld
