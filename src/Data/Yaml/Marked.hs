module Data.Yaml.Marked
  ( Marked
  , markedZero
  , markedItem
  , markedEvent
  , getMarkedItem
  , getMarkedPath
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
  , _markedPath :: FilePath
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

simplify :: Marked a -> (a, FilePath, (Int, Int, Int), (Int, Int, Int))
simplify (Marked i fp (YamlMark si sl sc) (YamlMark ei el ec)) =
  (i, fp, (si, sl, sc), (ei, el, ec))

markedZero :: a -> FilePath -> Marked a
markedZero a fp = markedItem a fp zeroYamlMark zeroYamlMark

markedItem :: a -> FilePath -> YamlMark -> YamlMark -> Marked a
markedItem a fp s e =
  Marked
    { _markedItem = a
    , _markedPath = fp
    , _markedStart = s
    , _markedEnd = e
    }

markedEvent :: FilePath -> MarkedEvent -> Marked Event
markedEvent fp MarkedEvent {..} =
  markedItem yamlEvent fp yamlStartMark yamlEndMark

getMarkedItem :: Marked a -> a
getMarkedItem = _markedItem

getMarkedPath :: Marked a -> FilePath
getMarkedPath = _markedPath

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
