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
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Yaml.Marked
import Data.Yaml.Marked.Value

withObject
  :: (MarkedObject -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withObject f mv = case getMarkedItem mv of
  Object hm -> do
    a <- f hm
    pure $ a <$ mv
  _ -> Left "typeMismatch"

withArray
  :: (MarkedArray -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withArray f mv = case getMarkedItem mv of
  Array v -> do
    a <- f v
    pure $ a <$ mv
  _ -> Left "typeMismatch"

withText
  :: (Text -> Either String a)
  -> Marked Value
  -> Either String (Marked a)
withText f mv = case getMarkedItem mv of
  String t -> do
    a <- f t
    pure $ a <$ mv
  _ -> Left "typeMismatch"

(.:) :: MarkedObject -> Key -> Either String (Marked Value)
(.:) km k = maybe (Left "Key not found") Right $ KeyMap.lookup k km

text :: Marked Value -> Either String (Marked Text)
text = withText pure

array
  :: (Marked Value -> Either String (Marked a))
  -> Marked Value
  -> Either String (Marked [Marked a])
array f = withArray $ traverse f . toList
