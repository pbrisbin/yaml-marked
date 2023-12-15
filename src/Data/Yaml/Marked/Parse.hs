module Data.Yaml.Marked.Parse
  ( withObject
  , withArray
  , withText
  , withScientific
  , withBool
  , (.:)
  , array
  , json
  , text
  , double
  , int
  ) where

import Prelude

import Data.Aeson (FromJSON, Key)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Yaml.Marked
import Data.Yaml.Marked.Value

withObject
  :: String
  -> (MarkedObject -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withObject label f = traverse $ \case
  Object hm -> f hm
  v -> prependContext label $ typeMismatch "Object" v

withArray
  :: String
  -> (MarkedArray -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withArray label f = traverse $ \case
  Array v -> f v
  v -> prependContext label $ typeMismatch "Array" v

withText
  :: String
  -> (Text -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withText label f = traverse $ \case
  String t -> f t
  v -> prependContext label $ typeMismatch "String" v

withScientific
  :: String
  -> (Scientific -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withScientific label f = traverse $ \case
  Number s -> f s
  v -> prependContext label $ typeMismatch "Number" v

withBool
  :: String
  -> (Bool -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withBool label f = traverse $ \case
  Bool b -> f b
  v -> prependContext label $ typeMismatch "Bool" v

prependContext :: String -> Either String a -> Either String a
prependContext label = first (prefix <>)
 where
  prefix = "parsing " <> label <> " failed, "

typeMismatch :: String -> Value -> Either String a
typeMismatch expected =
  Left . (prefix <>) . \case
    Object {} -> "Object"
    Array {} -> "Array"
    String {} -> "String"
    Number {} -> "Number"
    Bool {} -> "Bool"
    Null -> "Null"
 where
  prefix = "expected " <> expected <> ", but encountered "

(.:) :: MarkedObject -> Key -> Either String (Marked Value)
(.:) km k = maybe (Left "Key not found") Right $ KeyMap.lookup k km

array
  :: (Marked Value -> Either String (Marked a))
  -> Marked Value
  -> Either String (Marked [Marked a])
array f = withArray "an array" $ traverse f . toList

-- | Parse the value using its 'FromJSON' instance, passing along the marks
json :: FromJSON a => Marked Value -> Either String (Marked a)
json = traverse valueAsJSON

text :: Marked Value -> Either String (Marked Text)
text = json

double :: Marked Value -> Either String (Marked Double)
double = json

int :: Marked Value -> Either String (Marked Int)
int = json
