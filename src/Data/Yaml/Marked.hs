module Data.Yaml.Marked
  ( Marked
  , markedZero
  , markedItem
  , markedEvent
  , getMarkedItem
  , getMarkedLength
  , getMarkedIndexes
  , getMarkedStart
  , getMarkedStartIndex
  , getMarkedStartLine
  , getMarkedStartColumn
  , getMarkedEnd
  , getMarkedEndIndex
  , getMarkedEndLine
  , getMarkedEndColumn
  , YamlMark (..)
  ) where

import Prelude

import Data.Function (on)
import Text.Libyaml (Event, MarkedEvent (..), YamlMark (..))

data Marked a = Marked
  { _markedItem :: a
  , _markedStart :: YamlMark
  , _markedEnd :: YamlMark
  }
  deriving stock (Functor, Foldable, Traversable)

-- by-hand required because YamlMark lacks Eq
instance Eq a => Eq (Marked a) where
  (==) = (==) `on` simplify

-- by-hand required because YamlMark lacks Show
instance Show a => Show (Marked a) where
  show = show . simplify

simplify :: Marked a -> (a, (Int, Int, Int), (Int, Int, Int))
simplify (Marked i (YamlMark si sl sc) (YamlMark ei el ec)) =
  (i, (si, sl, sc), (ei, el, ec))

markedZero :: a -> Marked a
markedZero a = markedItem a zeroYamlMark zeroYamlMark

markedItem :: a -> YamlMark -> YamlMark -> Marked a
markedItem a s e =
  Marked
    { _markedItem = a
    , _markedStart = s
    , _markedEnd = e
    }

markedEvent :: MarkedEvent -> Marked Event
markedEvent MarkedEvent {..} =
  markedItem yamlEvent yamlStartMark yamlEndMark

getMarkedItem :: Marked a -> a
getMarkedItem = _markedItem

getMarkedIndexes :: Marked a -> (Int, Int)
getMarkedIndexes = (,) <$> getMarkedStartIndex <*> getMarkedEndIndex

getMarkedLength :: Marked a -> Int
getMarkedLength = uncurry subtract . getMarkedIndexes

getMarkedStart :: Marked a -> YamlMark
getMarkedStart = _markedStart

getMarkedStartIndex :: Marked a -> Int
getMarkedStartIndex = yamlIndex . getMarkedStart

getMarkedStartLine :: Marked a -> Int
getMarkedStartLine = yamlLine . getMarkedStart

getMarkedStartColumn :: Marked a -> Int
getMarkedStartColumn = yamlColumn . getMarkedStart

getMarkedEnd :: Marked a -> YamlMark
getMarkedEnd = _markedEnd

getMarkedEndIndex :: Marked a -> Int
getMarkedEndIndex = yamlIndex . getMarkedEnd

getMarkedEndLine :: Marked a -> Int
getMarkedEndLine = yamlLine . getMarkedEnd

getMarkedEndColumn :: Marked a -> Int
getMarkedEndColumn = yamlColumn . getMarkedEnd

zeroYamlMark :: YamlMark
zeroYamlMark = YamlMark 0 0 0
