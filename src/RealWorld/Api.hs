{-# LANGUAGE OverloadedStrings #-}
module RealWorld.Api
  ( Api, server ) where

import Servant

import qualified RealWorld.Api.Articles       as Articles
import qualified RealWorld.Api.Authentication as Authentication
import qualified RealWorld.Api.Profiles       as Profiles
import qualified RealWorld.Api.Tags           as Tags
import qualified RealWorld.Api.User           as User
import           RealWorld.Monad

type Api =
  "users"    :> Authentication.Api
    :<|>
  "user"     :> User.Api
    :<|>
  "profiles" :> Profiles.Api
    :<|>
  "articles" :> Articles.Api
    :<|>
  "tags"     :> Tags.Api


server :: ServerT Api RealWorld
server =
  Authentication.server
    :<|>
  User.server
    :<|>
  Profiles.server
    :<|>
  Articles.server
    :<|>
  Tags.server
