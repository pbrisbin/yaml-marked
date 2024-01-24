module Data.Yaml.Marked
  ( Marked (..)
  , markAtZero
  , Location (..)
  , locationZero

    -- * Interop with "Data.Yaml"
  , MarkedEvent
  , fromMarkedEvent
  -- , YamlMark
  -- , locationFromYamlMark
  ) where

import Prelude

import Numeric.Natural
import Text.Libyaml (Event, MarkedEvent (..), YamlMark (..))

data Marked a = Marked
  { markedItem :: a
  , markedPath :: FilePath
  , markedLocationStart :: Location
  , markedLocationEnd :: Location
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data Location = Location
  { locationIndex :: Natural
  , locationLine :: Natural
  , locationColumn :: Natural
  }
  deriving stock (Eq, Show)

markAtZero :: a -> FilePath -> Marked a
markAtZero a fp =
  Marked
    { markedItem = a
    , markedPath = fp
    , markedLocationStart = locationZero
    , markedLocationEnd = locationZero
    }

locationZero :: Location
locationZero =
  Location
    { locationIndex = 0
    , locationLine = 0
    , locationColumn = 0
    }

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
    , markedLocationStart = locationFromYamlMark yamlStartMark
    , markedLocationEnd = locationFromYamlMark yamlEndMark
    }

int2nat :: Int -> Natural
int2nat = fromIntegral . max 0
