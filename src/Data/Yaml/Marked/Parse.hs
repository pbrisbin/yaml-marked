module Data.Yaml.Marked.Parse
  ( Parser
  , withObject
  , withArray
  , withText
  , withScientific
  , withBool
  , (.:)
  , (.:?)
  , array
  , json
  , value
  , text
  , double
  , int
  , bool
  ) where

import Prelude

import Data.Aeson.Compat (FromJSON, Key)
import qualified Data.Aeson.Compat as Aeson
import qualified Data.Aeson.Compat.KeyMap as KeyMap
import Data.Aeson.Types (Parser, prependFailure)
import Data.Foldable (toList)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Yaml.Marked
import Data.Yaml.Marked.Value

withObject
  :: String
  -> (MarkedObject -> Parser a)
  -> Marked Value
  -> Parser (Marked a)
withObject label f = traverse $ \case
  Object hm -> f hm
  v -> prependContext label $ typeMismatch "Object" v

withArray
  :: String
  -> (MarkedArray -> Parser a)
  -> Marked Value
  -> Parser (Marked a)
withArray label f = traverse $ \case
  Array v -> f v
  v -> prependContext label $ typeMismatch "Array" v

withText
  :: String
  -> (Text -> Parser a)
  -> Marked Value
  -> Parser (Marked a)
withText label f = traverse $ \case
  String t -> f t
  v -> prependContext label $ typeMismatch "String" v

withScientific
  :: String
  -> (Scientific -> Parser a)
  -> Marked Value
  -> Parser (Marked a)
withScientific label f = traverse $ \case
  Number s -> f s
  v -> prependContext label $ typeMismatch "Number" v

withBool
  :: String
  -> (Bool -> Parser a)
  -> Marked Value
  -> Parser (Marked a)
withBool label f = traverse $ \case
  Bool b -> f b
  v -> prependContext label $ typeMismatch "Bool" v

prependContext :: String -> Parser a -> Parser a
prependContext label = prependFailure prefix
 where
  prefix = "parsing " <> label <> " failed, "

typeMismatch :: String -> Value -> Parser a
typeMismatch expected =
  fail . (prefix <>) . \case
    Object {} -> "Object"
    Array {} -> "Array"
    String {} -> "String"
    Number {} -> "Number"
    Bool {} -> "Bool"
    Null -> "Null"
 where
  prefix = "expected " <> expected <> ", but encountered "

(.:) :: MarkedObject -> Key -> Parser (Marked Value)
(.:) km k = maybe (fail "Key not found") pure $ KeyMap.lookup k km

(.:?) :: MarkedObject -> Key -> Parser (Maybe (Marked Value))
(.:?) km k = pure $ KeyMap.lookup k km

array
  :: (Marked Value -> Parser (Marked a))
  -> Marked Value
  -> Parser (Marked [Marked a])
array f = withArray "an array" $ traverse f . toList

-- | Parse the value using its 'FromJSON' instance, passing along the marks
json :: FromJSON a => Marked Value -> Parser (Marked a)
json = traverse valueAsJSON

value :: Marked Value -> Parser (Marked Aeson.Value)
value = json

text :: Marked Value -> Parser (Marked Text)
text = json

double :: Marked Value -> Parser (Marked Double)
double = json

int :: Marked Value -> Parser (Marked Int)
int = json

bool :: Marked Value -> Parser (Marked Bool)
bool = json
