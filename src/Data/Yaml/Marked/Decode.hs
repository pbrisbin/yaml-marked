module Data.Yaml.Marked.Decode
  ( decodeThrow
  , decodeAllThrow
  , decodeFileEither
  , decodeAllFileEither
  , decodeFileWithWarnings
  , decodeAllFileWithWarnings

    -- * Exceptions
  , ParseException (..)
  , YamlException (..)
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadThrow, throwM)
import Data.ByteString (ByteString)
import Data.Yaml (ParseException (..))
import Data.Yaml.Marked
import Data.Yaml.Marked.Internal
import Data.Yaml.Marked.Value
import System.IO.Unsafe (unsafePerformIO)
import Text.Libyaml (YamlException (..))
import qualified Text.Libyaml as Y

decodeThrow
  :: MonadThrow m
  => (Marked Value -> Either String a)
  -> ByteString
  -> m a
decodeThrow p = either throwM pure . decodeEither p

decodeAllThrow
  :: MonadThrow m
  => (Marked Value -> Either String a)
  -> ByteString
  -> m [a]
decodeAllThrow p = either throwM pure . decodeAllEither p

decodeFileEither
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> FilePath
  -> m (Either ParseException a)
decodeFileEither p = fmap (fmap snd) . decodeFileWithWarnings p

decodeAllFileEither
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> FilePath
  -> m (Either ParseException [a])
decodeAllFileEither p = fmap (fmap snd) . decodeAllFileWithWarnings p

decodeFileWithWarnings
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> FilePath
  -> m (Either ParseException ([Warning], a))
decodeFileWithWarnings p = liftIO . decodeHelper p . Y.decodeFileMarked

decodeAllFileWithWarnings
  :: MonadIO m
  => (Marked Value -> Either String a)
  -> FilePath
  -> m (Either ParseException ([Warning], [a]))
decodeAllFileWithWarnings p = liftIO . decodeAllHelper p . Y.decodeFileMarked

decodeEither
  :: (Marked Value -> Either String a)
  -> ByteString
  -> Either ParseException a
decodeEither p =
  fmap snd . unsafePerformIO . decodeHelper p . Y.decodeMarked

decodeAllEither
  :: (Marked Value -> Either String a)
  -> ByteString
  -> Either ParseException [a]
decodeAllEither p =
  fmap snd . unsafePerformIO . decodeAllHelper p . Y.decodeMarked
