{-# LANGUAGE OverloadedStrings #-}
module RealWorld.Model.Field
  ( Field(..)
  , isValue
  , isNil
  , isUndefined
  , toValue
  , objectKey
  , require
  ) where

import RealWorld.Prelude

import           Data.Aeson         ((.!=), (.:!))
import qualified Data.Aeson         as Json
import qualified Data.Aeson.Types   as Json
import           Data.SafeCopy      (SafeCopy(..))
import           Data.SafeCopy      as SafeCopy
import qualified Data.Serialize.Get as Serialize
import qualified Data.Serialize.Put as Serialize
import qualified Data.Text          as Text

import RealWorld.Model.Errors

data Field (name :: Symbol) a
  = Value a
  | Nil
  | Undefined
  deriving (Show, Eq, Ord, Functor, Foldable)

instance SafeCopy a => SafeCopy (Field name a) where
  putCopy Nil = SafeCopy.contain $ do
    Serialize.putWord8 0
  putCopy Undefined = SafeCopy.contain $ do
    Serialize.putWord8 1
  putCopy (Value a) = SafeCopy.contain $ do
    Serialize.putWord8 2
    SafeCopy.safePut a
  getCopy = SafeCopy.contain $ do
    tag <- Serialize.getWord8
    case tag of
      0 -> pure Nil
      1 -> pure Undefined
      2 -> Value <$> SafeCopy.safeGet
      _ -> fail "SafeCopy Field: invalid tag"

instance Applicative (Field name) where
  pure = Value
  Value f   <*> a = f <$> a
  Nil       <*> _ = Nil
  Undefined <*> _ = Undefined

instance Alternative (Field name) where
  empty = Undefined
  Value a   <|> _ = Value a
  Nil       <|> _ = Nil
  Undefined <|> a = a

instance Monad (Field name) where
  Value a   >>= f = f a
  Nil       >>= _ = Nil
  Undefined >>= _ = Undefined

instance MonadPlus (Field name)

instance Json.FromJSON a => Json.FromJSON (Field name a) where
  parseJSON Json.Null = pure Nil
  parseJSON a         = Value <$> Json.parseJSON a

instance Json.ToJSON a => Json.ToJSON (Field name a) where
  toJSON (Value a) = Json.toJSON a
  toJSON Nil       = Json.Null
  toJSON Undefined = Json.Null

isValue :: Field name a -> Bool
isValue Value{} = True
isValue _       = False

isNil :: Field name a -> Bool
isNil Nil = True
isNil _   = False

isUndefined :: Field name a -> Bool
isUndefined Undefined = True
isUndefined _         = False

toValue :: Alternative m => Field name a -> m a
toValue (Value a) = pure a
toValue _         = empty

objectKey
  :: forall name a. (Json.FromJSON a, KnownSymbol name)
  => Json.Object -> Json.Parser (Field name a)
objectKey o = o .:! k .!= Undefined
  where
    k = Text.pack $ symbolVal @name Proxy

require
  :: forall name a m b. (MonadError Errors m, Alternative m, KnownSymbol name)
  => a -> (a -> Field name b) -> m b
require a f =
  toValue (f a)
    <|> throwError
          (unprocessable $ "Field " <> toUrlPiece name <> " is required.")
  where
    name = symbolVal @name Proxy


