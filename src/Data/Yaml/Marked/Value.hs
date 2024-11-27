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
import Data.Aeson.Compat.KeyMap (KeyMap)
import Data.Aeson.Types (IResult (..), Parser, iparse)
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

parseEither :: (a -> Parser b) -> a -> Either String b
parseEither f a = case iparse f a of
  IError _ x -> Left x
  ISuccess b -> Right b

-- | Convert a 'Value' to an equivalent 'Data.Aeson.Value'
valueToValue :: Value -> Aeson.Value
valueToValue = \case
  Object km -> Aeson.Object $ valueToValue . markedItem <$> km
  Array v -> Aeson.Array $ valueToValue . markedItem <$> v
  String x -> Aeson.String x
  Number x -> Aeson.Number x
  Bool x -> Aeson.Bool x
  Null -> Aeson.Null
