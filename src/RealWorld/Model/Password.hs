{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module RealWorld.Model.Password
  ( Password(..), PasswordHash
  , computeHash, verify
  ) where

import RealWorld.Prelude

import qualified Crypto.PasswordStore as PasswordStore
import           Data.SafeCopy        (base, deriveSafeCopy)
import qualified Data.Text.Encoding   as Text

newtype Password = Password Text
  deriving (FromJSON)

newtype PasswordHash = PasswordHash ByteString
  deriving (Show, Eq, Ord)

$(deriveSafeCopy 0 'base ''PasswordHash)

computeHash :: MonadIO m => Password -> m PasswordHash
computeHash (Password plain) = liftIO $ do
  let bytes = Text.encodeUtf8 plain
  PasswordHash <$> PasswordStore.makePassword bytes 17

verify :: Password -> PasswordHash -> Bool
verify (Password plain) (PasswordHash hashed) =
  PasswordStore.verifyPassword
    (Text.encodeUtf8 plain)
    hashed
