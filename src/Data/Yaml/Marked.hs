module Data.Yaml.Marked
  ( Marked (..)
  , Location (..)

    -- * Interop with "Data.Yaml"
  , MarkedEvent
  , fromMarkedEvent
  ) where

import Prelude

import Data.Aeson (JSONPath)
import Numeric.Natural
import Text.Libyaml (Event, MarkedEvent (..), YamlMark (..))

data Marked a = Marked
  { markedItem :: a
  , markedPath :: FilePath
  , markedJSONPath :: Maybe JSONPath
  , markedLocationStart :: Location
  -- ^ Location of the first character of the item
  --
  -- In the following example, marking @value@ will have a start location:
  --
  -- @
  -- key: value\n
  --      ^-- HERE
  -- @
  , markedLocationEnd :: Location
  -- ^ Location of the first character /after/ the item
  --
  -- In the following example, marking @value@ will have an end location:
  --
  -- @
  -- key: value\n
  --           ^-- HERE
  -- @
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | Index, line, and column of a character
--
-- NB. All values are 0-based.
data Location = Location
  { locationIndex :: Natural
  , locationLine :: Natural
  , locationColumn :: Natural
  }
  deriving stock (Eq, Show)

locationFromYamlMark :: YamlMark -> Location
locationFromYamlMark YamlMark {..} =
  Location
    { locationIndex = int2nat yamlIndex
    , locationLine = int2nat yamlLine
    , locationColumn = int2nat yamlColumn
    }

fromMarkedEvent :: MarkedEvent -> FilePath -> Marked Event
fromMarkedEvent MarkedEvent {..} fp =
  Marked
    { markedItem = yamlEvent
    , markedPath = fp
    , markedJSONPath = Nothing
    , markedLocationStart = locationFromYamlMark yamlStartMark
    , markedLocationEnd = locationFromYamlMark yamlEndMark
    }

int2nat :: Int -> Natural
int2nat = fromIntegral . max 0
