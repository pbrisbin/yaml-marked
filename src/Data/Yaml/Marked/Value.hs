module Data.Yaml.Marked.Value
  ( Value (..)
  , MarkedObject
  , MarkedArray
  , markedValueToValue
  , markedValueAsJSON
  ) where

import Prelude

import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (parseEither)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Yaml.Marked

data Value
  = Object !MarkedObject
  | Array !MarkedArray
  | String !Text
  | Number !Scientific
  | Bool !Bool
  | Null
  deriving stock (Show)

type MarkedObject = KeyMap (Marked Value)

type MarkedArray = Vector (Marked Value)

markedValueToValue :: Marked Value -> Aeson.Value
markedValueToValue = go . getMarkedItem
 where
  go = \case
    Object km -> Aeson.Object $ markedValueToValue <$> km
    Array v -> Aeson.Array $ markedValueToValue <$> v
    String x -> Aeson.String x
    Number x -> Aeson.Number x
    Bool x -> Aeson.Bool x
    Null -> Aeson.Null

markedValueAsJSON :: FromJSON a => Marked Value -> Either String a
markedValueAsJSON = parseEither parseJSON . markedValueToValue
