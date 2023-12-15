module Data.Yaml.Marked.Replace
  ( Replace
  , newReplace
  , replaceMarked
  , ReplaceException (..)
  , runReplaces
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.Trans.Resource (MonadThrow (..))
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import Data.Yaml.Marked
import UnliftIO.Exception (Exception (..))

data Replace = Replace
  { replaceIndex :: Int
  , replacedLength :: Int
  , replacedBy :: ByteString
  }
  deriving stock (Eq, Show)

-- | Create a 'Replace' directly at given index and length
newReplace :: Int -> Int -> ByteString -> Replace
newReplace idx len bs =
  Replace
    { replaceIndex = idx
    , replacedLength = len
    , replacedBy = bs
    }

-- | Create a 'Replace' for something 'Marked'
replaceMarked :: Marked a -> ByteString -> Replace
replaceMarked m = newReplace (getMarkedStartIndex m) (getMarkedLength m)

data ReplaceException
  = NegativeStartIndex Replace
  | NegativeLength Replace
  | ReplaceOutOfBounds Replace Int
  | OverlappingReplace Replace
  deriving stock (Eq, Show)

instance Exception ReplaceException where
  displayException = \case
    NegativeStartIndex r ->
      "The replacement " <> show r <> " has a negative start index"
    NegativeLength r ->
      "The replacement " <> show r <> " has negative length"
    ReplaceOutOfBounds r bLen ->
      "The replacement "
        <> show r
        <> " is trying to replace more characters than remain in the ByteString ("
        <> show bLen
        <> ")"
    OverlappingReplace r ->
      "The replacement "
        <> show r
        <> " is where an earlier replacement has already been made"

runReplaces :: MonadThrow m => [Replace] -> ByteString -> m ByteString
runReplaces = go 0 "" . sortOn replaceIndex
 where
  go _ acc [] bs = pure $ acc <> bs
  go offset acc (r : rs) bs = do
    (before, after) <- breakAtOffsetReplace offset r bs
    let newOffset = offset + BS8.length before + replacedLength r
    go newOffset (acc <> before <> replacedBy r) rs after

-- | Break a 'ByteString' into the content before/after a replacement
--
-- Will throw 'ReplaceException' if the 'Replace' is not valid for the given
-- input.
breakAtOffsetReplace
  :: MonadThrow m
  => Int
  -- ^ An amount to shift the 'replaceIndex' by
  --
  -- Since this function is called recursively to incrementally replace within
  -- an overall 'ByteString', to which the 'replaceIndex' is relative, we need
  -- to track how much to shift it as we recur.
  -> Replace
  -> ByteString
  -> m (ByteString, ByteString)
breakAtOffsetReplace offset r bs = do
  when (sIdx < 0) $
    throwM $
      -- A negative index post-recursion (offset != 0) means a later replacement
      -- has landed within something we already replaced. Otherwise, it was just
      -- negative to begin with.
      if offset == 0
        then NegativeStartIndex r
        else OverlappingReplace r

  when (rLen < 0) $ throwM $ NegativeLength r
  when (rLen > bLen) $ throwM $ ReplaceOutOfBounds r bLen
  pure $ second (BS8.drop rLen) $ BS8.splitAt sIdx bs
 where
  sIdx = replaceIndex r - offset
  rLen = replacedLength r
  bLen = BS8.length bs
