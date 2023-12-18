module Data.Yaml.Marked.Value
  ( Value (..)
  , MarkedObject
  , MarkedArray
  , valueAsJSON
  , valueToValue
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
  deriving stock (Eq, Show)

type MarkedObject = KeyMap (Marked Value)

type MarkedArray = Vector (Marked Value)

-- | Parse the value using its 'FromJSON', discarding any marks
valueAsJSON :: FromJSON a => Value -> Either String a
valueAsJSON = parseEither parseJSON . valueToValue

-- | Convert a 'Value' to an equivalent 'Data.Aeson.Value'
valueToValue :: Value -> Aeson.Value
valueToValue = \case
  Object km -> Aeson.Object $ valueToValue . getMarkedItem <$> km
  Array v -> Aeson.Array $ valueToValue . getMarkedItem <$> v
  String x -> Aeson.String x
  Number x -> Aeson.Number x
  Bool x -> Aeson.Bool x
  Null -> Aeson.Null
