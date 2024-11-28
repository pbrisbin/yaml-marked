module Data.Yaml.Marked.Replace
  ( Replace
  , newReplace
  , replaceMarked
  , ReplaceException (..)
  , runReplaces
  , runReplacesOnOverlapping
  ) where

import Prelude

import Control.Monad (void, when)
import Control.Monad.Trans.Resource (MonadThrow (..))
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import Data.Yaml.Marked
import Numeric.Natural
import UnliftIO.Exception (Exception (..))

data Replace = Replace
  { replaceIndex :: Natural
  , replacedLength :: Natural
  , replacedBy :: ByteString
  }
  deriving stock (Eq, Show)

-- | Create a 'Replace' directly at given index and length
--
-- NB. This function is unsafe in that it can be used with negative literals and
-- will fail at runtime. Prefer 'replaceMarked'.
newReplace :: Natural -> Natural -> ByteString -> Replace
newReplace idx len bs =
  Replace
    { replaceIndex = idx
    , replacedLength = len
    , replacedBy = bs
    }

-- | Create a 'Replace' for something 'Marked'
replaceMarked :: Marked a -> ByteString -> Replace
replaceMarked Marked {..} = newReplace idx len
 where
  idx = locationIndex markedLocationStart
  end = locationIndex markedLocationEnd
  len
    | end >= idx = end - idx
    | otherwise = 0

data ReplaceException
  = ReplaceOutOfBounds Replace Natural
  | OverlappingReplace Replace
  deriving stock (Eq, Show)

instance Exception ReplaceException where
  displayException = \case
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
runReplaces = runReplacesOnOverlapping $ throwM . OverlappingReplace

runReplacesOnOverlapping
  :: MonadThrow m
  => (Replace -> m a)
  -- ^ What to do with the first overlapping 'Replace' if encountered
  --
  -- NB. the overlapping replace(s) will be ignored, but this allows you to log
  -- it as a warning, or use 'throwM' to halt (which 'runReplaces' does).
  -> [Replace]
  -> ByteString
  -> m ByteString
runReplacesOnOverlapping f rs bs = do
  rs' <- filterOverlapping f $ sortOn replaceIndex rs
  runReplaces' 0 "" rs' bs

runReplaces'
  :: MonadThrow m
  => Natural
  -> ByteString
  -> [Replace]
  -> ByteString
  -> m ByteString
runReplaces' _ acc [] bs = pure $ acc <> bs
runReplaces' offset acc (r : rs) bs = do
  (before, after) <- breakAtOffsetReplace offset r bs
  let newOffset = offset + fromIntegral (BS8.length before) + replacedLength r
  runReplaces' newOffset (acc <> before <> replacedBy r) rs after

filterOverlapping
  :: Monad m => (Replace -> m a) -> [Replace] -> m [Replace]
filterOverlapping onOverlap = go []
 where
  go acc [] = pure acc
  go acc (r : rs)
    | any (r `precedesEndOf`) acc = do
        void $ onOverlap r
        go acc rs
    | otherwise = go (acc <> [r]) rs

precedesEndOf :: Replace -> Replace -> Bool
precedesEndOf a b = replaceIndex a < replaceIndex b + replacedLength b

-- | Break a 'ByteString' into the content before/after a replacement
--
-- Will throw 'ReplaceException' if the 'Replace' is not valid for the given
-- input.
breakAtOffsetReplace
  :: MonadThrow m
  => Natural
  -- ^ An amount to shift the 'replaceIndex' by
  --
  -- Since this function is called recursively to incrementally replace within
  -- an overall 'ByteString', to which the 'replaceIndex' is relative, we need
  -- to track how much to shift it as we recur.
  -> Replace
  -> ByteString
  -> m (ByteString, ByteString)
breakAtOffsetReplace offset r bs = do
  when (rLen > bLen) $ throwM $ ReplaceOutOfBounds r bLen
  pure $
    second (BS8.drop $ fromIntegral rLen) $
      BS8.splitAt (fromIntegral sIdx) bs
 where
  sIdx
    | offset > replaceIndex r = error "TODO"
    | otherwise = replaceIndex r - offset
  rLen = replacedLength r
  bLen = fromIntegral $ BS8.length bs
