module Data.Yaml.Marked.Parse
  ( withObject
  , withArray
  , withText
  , (.:)
  , text
  , array
  ) where

import Prelude

import Data.Aeson (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Yaml.Marked
import Data.Yaml.Marked.Value

withObject
  :: String
  -> (MarkedObject -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withObject label f mv = case getMarkedItem mv of
  Object hm -> do
    a <- f hm
    pure $ a <$ mv
  v -> prependContext label $ typeMismatch "Object" v

withArray
  :: String
  -> (MarkedArray -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withArray label f mv = case getMarkedItem mv of
  Array v -> do
    a <- f v
    pure $ a <$ mv
  v -> prependContext label $ typeMismatch "Array" v

withText
  :: String
  -> (Text -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withText label f mv = case getMarkedItem mv of
  String t -> do
    a <- f t
    pure $ a <$ mv
  v -> prependContext label $ typeMismatch "String" v

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

text :: Marked Value -> Either String (Marked Text)
text = withText "a text" pure

array
  :: (Marked Value -> Either String (Marked a))
  -> Marked Value
  -> Either String (Marked [Marked a])
array f = withArray "an array" $ traverse f . toList
