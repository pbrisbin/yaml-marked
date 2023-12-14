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
decodeThrow p = either throwM pure . decodeEither' p

decodeAllThrow
  :: MonadThrow m
  => (Marked Value -> Either String a)
  -> ByteString
  -> m [a]
decodeAllThrow p = either throwM pure . decodeAllEither' p

decodeFileEither
  :: (Marked Value -> Either String a)
  -> FilePath
  -> IO (Either ParseException a)
decodeFileEither p = fmap (fmap snd) . decodeFileWithWarnings p

decodeAllFileEither
  :: (Marked Value -> Either String a)
  -> FilePath
  -> IO (Either ParseException [a])
decodeAllFileEither p = fmap (fmap snd) . decodeAllFileWithWarnings p

decodeFileWithWarnings
  :: (Marked Value -> Either String a)
  -> FilePath
  -> IO (Either ParseException ([Warning], a))
decodeFileWithWarnings p = decodeHelper_ p . Y.decodeFileMarked

decodeAllFileWithWarnings
  :: (Marked Value -> Either String a)
  -> FilePath
  -> IO (Either ParseException ([Warning], [a]))
decodeAllFileWithWarnings p = decodeAllHelper_ p . Y.decodeFileMarked

decodeEither'
  :: (Marked Value -> Either String a)
  -> ByteString
  -> Either ParseException a
decodeEither' p =
  either Left (either (Left . AesonException) Right)
    . unsafePerformIO
    . fmap (fmap snd)
    . decodeHelper p
    . Y.decodeMarked

decodeAllEither'
  :: (Marked Value -> Either String a)
  -> ByteString
  -> Either ParseException [a]
decodeAllEither' p =
  either Left (either (Left . AesonException) Right)
    . unsafePerformIO
    . fmap (fmap snd)
    . decodeAllHelper p
    . Y.decodeMarked
