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
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Yaml.Marked
import Data.Yaml.Marked.Internal
import Data.Yaml.Marked.ParseException (ParseException (..))
import Data.Yaml.Marked.Value
import System.IO.Unsafe (unsafePerformIO)
import Text.Libyaml (YamlException (..))
import qualified Text.Libyaml as Y

decodeThrow
  :: MonadThrow m
  => (Marked Value -> Parser a)
  -> FilePath
  -- ^ Name of input being parsed
  -> ByteString
  -> m a
decodeThrow p fp = either throwM pure . decodeEither p fp

decodeAllThrow
  :: MonadThrow m
  => (Marked Value -> Parser a)
  -> FilePath
  -- ^ Name of input being parsed
  -> ByteString
  -> m [a]
decodeAllThrow p fp = either throwM pure . decodeAllEither p fp

decodeFileEither
  :: MonadIO m
  => (Marked Value -> Parser a)
  -> FilePath
  -> m (Either ParseException a)
decodeFileEither p = fmap (fmap fst) . decodeFileWithWarnings p

decodeAllFileEither
  :: MonadIO m
  => (Marked Value -> Parser a)
  -> FilePath
  -> m (Either ParseException [a])
decodeAllFileEither p = fmap (fmap fst) . decodeAllFileWithWarnings p

decodeFileWithWarnings
  :: MonadIO m
  => (Marked Value -> Parser a)
  -> FilePath
  -> m (Either ParseException (a, [Warning]))
decodeFileWithWarnings p fp = liftIO $ decodeHelper p fp $ Y.decodeFileMarked fp

decodeAllFileWithWarnings
  :: MonadIO m
  => (Marked Value -> Parser a)
  -> FilePath
  -> m (Either ParseException ([a], [Warning]))
decodeAllFileWithWarnings p fp =
  liftIO $ decodeAllHelper p fp $ Y.decodeFileMarked fp

decodeEither
  :: (Marked Value -> Parser a)
  -> FilePath
  -- ^ Name of input being parsed
  -> ByteString
  -> Either ParseException a
decodeEither p fp =
  fmap fst . unsafePerformIO . decodeHelper p fp . Y.decodeMarked

decodeAllEither
  :: (Marked Value -> Parser a)
  -> FilePath
  -- ^ Name of input being parsed
  -> ByteString
  -> Either ParseException [a]
decodeAllEither p fp =
  fmap fst . unsafePerformIO . decodeAllHelper p fp . Y.decodeMarked
