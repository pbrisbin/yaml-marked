-- | Operations for modifying a one-line marked
module Data.Yaml.Marked.OneLine
  ( OneLineMarked
  , newOneLineMarked
  , getOneLineMarked
  , Modify (..)
  , modify
  ) where

import Prelude

import Data.Yaml.Marked

data OneLineMarked a = OneLineMarked
  { item :: a
  , path :: FilePath
  , line :: Natural
  , startIndex :: Natural
  , startColumn :: Natural
  , endIndex :: Natural
  , endColumn :: Natural
  }

newOneLineMarked :: Marked a -> Either String (OneLineMarked a)
newOneLineMarked m
  | startLine /= endLine = Left ""
  | otherwise =
      OneLineMarked
        { item = markedItem m
        , line = startLine
        , startIndex = locationIndex $ markedLocationStart m
        , startColumn = locationColumn $ markedLocationStart m
        , endIndex = locationIndex $ markedLocationEnd m
        , endColumn = locationColumn $ markedLocationEnd m
        }
 where
  startLine = locationLine $ markedLocationStart m
  endLine = locationLine $ markedLocationEnd m

getOneLineMarked :: OneLineMarked a -> Marked a
getOneLineMarked olm = Marked
  { markedItem = item olm
  , markedPath = path olm
  , markedLocationStart = Location
    { locationIndex = startIndex olm
    , locationLine = line olm
    , locationColumn = startColumn olm
    }
  , markedLocationEnd = Location
    { locationIndex = startIndex olm
    , locationLine = line olm
    , locationColumn = startColumn olm
    }
  }

data Modify
  = Shrink Natural Natural
  | ShrinkLeft Natural
  | ShrinkRight Natural
  | Expand Natural Natural
  | ExpandLeft Natural
  | ExpandRight Natural

modify :: Modify -> OneLineMarked a -> Either String (OneLineMarked a)
modify = \case
  Shrink l r -> \olm -> do
    let
      oldLI = startIndex olm
      oldLC = startColumn olm

      oldRI = endIndex olm
      oldRC = endColumn olm

      newLI = oldLI `plusNatural` l
      newLC = oldLC `plusNatural` l

    newRI <- note "End index would be negative" $ oldRI `minusNaturalMaybe` r
    newRC <- note "End column would be negative" $ oldRC `minusNaturalMaybe` r

    when (newRI < newLI) $ Left "End index would be before start"
    when (newRI > newLC) $ Left "End column would be before start"

    Right
      olm
        { startIndex = newLI
        , startColumn = newLC
        , endIndex = newRI
        , endColumn = newRC
        }
  ShrinkLeft l -> modify $ Shrink l 0
  ShrinkRight r -> modify $ Shrink 0 r
  Expand l r -> \olm -> do
    let
      oldLI = startIndex olm
      oldLC = startColumn olm

      oldRI = endIndex olm
      oldRC = endColumn olm

      newRC = oldLR `plusNatural` r
      newRI = oldRI `plusNatural` r

    newLI <- note "Start index would be negative" $ oldLI `minusNaturalMaybe` l
    newLC <- note "Start column would be negative" $ oldLC `minusNaturalMaybe` l

    Right
      olm
        { startIndex = newLI
        , startColumn = newLC
        , endIndex = newRI
        , endColumn = newRC
        }
  ExpandLeft l -> modify $ Expand l 0
  ExpandRight r -> modify $ Expand 0 r
