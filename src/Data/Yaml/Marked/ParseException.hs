module Data.Yaml.Marked.ParseException
  ( ParseException (..)
  , YamlException (..)
  , displayParseException
  ) where

import Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml (ParseException (..), prettyPrintParseException)
import Text.Libyaml (YamlException (..))

displayParseException :: Maybe ByteString -> ParseException -> String
displayParseException = \case
  Nothing -> prettyPrintParseException
  Just bs -> prettyPrintParseExceptionInSource bs

prettyPrintParseExceptionInSource :: ByteString -> ParseException -> String
prettyPrintParseExceptionInSource bs = \case
  -- InvalidYaml (Just ex) -> undefined
  ex ->
    unlines
      [ prettyPrintParseException ex
      , "Input:"
      , BS8.unpack $ BS8.unlines $ map ("  | " <>) $ BS8.lines bs
      , ""
      ]
