{-# LANGUAGE TemplateHaskell #-}
module RealWorld.Model.Field
  ( Field(..)
  , isValue
  , isNil
  , isUndefined
  , toMaybe
  , getJson
  ) where

import RealWorld.Prelude

import Data.SafeCopy (base, deriveSafeCopy)

import           Data.Aeson       ((.!=), (.:!))
import qualified Data.Aeson       as Json
import qualified Data.Aeson.Types as Json

data Field a
  = Value a
  | Nil
  | Undefined
  deriving (Show, Eq, Ord, Functor, Foldable)

$(deriveSafeCopy 0 'base ''Field)

instance Applicative Field where
  pure = Value
  Value f   <*> a = f <$> a
  Nil       <*> _ = Nil
  Undefined <*> _ = Undefined

instance Alternative Field where
  empty = Undefined
  Value a   <|> _         = Value a
  Nil       <|> Value b   = Value b
  Nil       <|> Undefined = Nil
  Nil       <|> Nil       = Nil
  Undefined <|> a         = a

instance Monad Field where
  Value a   >>= f = f a
  Nil       >>= _ = Nil
  Undefined >>= _ = Undefined

instance MonadPlus Field

instance Json.FromJSON a => Json.FromJSON (Field a) where
  parseJSON Json.Null = pure Nil
  parseJSON a         = Value <$> Json.parseJSON a

instance Json.ToJSON a => Json.ToJSON (Field a) where
  toJSON (Value a) = Json.toJSON a
  toJSON Nil       = Json.Null
  toJSON Undefined = Json.Null

isValue :: Field a -> Bool
isValue Value{} = True
isValue _       = False

isNil :: Field a -> Bool
isNil Nil = True
isNil _   = False

isUndefined :: Field a -> Bool
isUndefined Undefined = True
isUndefined _         = False

toMaybe :: Field a -> Maybe a
toMaybe (Value a)   = Just a
toMaybe Nil         = Nothing
toMaybe Undefined   = Nothing

getJson :: Json.FromJSON a => Json.Object -> Text -> Json.Parser (Field a)
getJson o k = o .:! k .!= Undefined
