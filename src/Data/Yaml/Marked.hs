module Data.Yaml.Marked
  ( Marked
  , markedZero
  , markedItem
  , getMarkedItem
  , getMarkedStart
  , getMarkedStartIndex
  , getMarkedEnd
  , getMarkedEndIndex
  , YamlMark (..)
  ) where

import Prelude

import Data.Function (on)
import Text.Libyaml (YamlMark (..))

data Marked a = Marked
  { _markedItem :: a
  , _markedStart :: YamlMark
  , _markedEnd :: YamlMark
  }
  deriving stock (Functor)

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

getMarkedItem :: Marked a -> a
getMarkedItem = _markedItem

getMarkedStart :: Marked a -> YamlMark
getMarkedStart = _markedStart

getMarkedStartIndex :: Marked a -> Int
getMarkedStartIndex = yamlIndex . getMarkedStart

getMarkedEnd :: Marked a -> YamlMark
getMarkedEnd = _markedEnd

getMarkedEndIndex :: Marked a -> Int
getMarkedEndIndex = yamlIndex . getMarkedEnd

zeroYamlMark :: YamlMark
zeroYamlMark = YamlMark 0 0 0
