module Data.Yaml.Marked.Replace
  ( Replace
  , newReplace
  , runReplace
  , Replaces
  , newReplaces
  , runReplaces
  ) where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad (guard, void)
import Control.Monad.Trans.Resource (MonadThrow (..))
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Yaml.Marked
import qualified Rampart
import UnliftIO.Exception (Exception)

data ReplaceException
  = InvalidMarked (Marked ()) String
  | OverlappingReplaces (NonEmpty (Replace, Replace))
  deriving stock (Show)
  deriving anyclass (Exception)

data Replace = Replace
  { replaceIndex :: Int
  , replacedLength :: Int
  , replacedBy :: ByteString
  }
  deriving stock (Show)

newReplace :: MonadThrow m => Marked a -> ByteString -> m Replace
newReplace m bs =
  either (throwM . InvalidMarked (void m)) pure $ do
    si <- note "Negative start" $ guarded (>= 0) $ getMarkedStartIndex m
    ei <- note "End before start" $ guarded (>= si) $ getMarkedEndIndex m
    pure $
      Replace
        { replaceIndex = si
        , replacedLength = ei - si
        , replacedBy = bs
        }

runReplace :: Replace -> ByteString -> ByteString
runReplace = runReplaces . Replaces . pure

newtype Replaces = Replaces
  { unReplaces :: [Replace]
  }

newReplaces :: MonadThrow m => [Replace] -> m Replaces
newReplaces rs
  | Just x <- mOverlaps = throwM $ OverlappingReplaces x
  | otherwise = pure $ Replaces rs
 where
  mOverlaps =
    NE.nonEmpty
      [ (a, b)
      | a <- rs
      , b <- rs
      , a `replaceOverlaps` b
      ]

runReplaces :: Replaces -> ByteString -> ByteString
runReplaces = go 0 "" . unReplaces
 where
  go :: Int -> ByteString -> [Replace] -> ByteString -> ByteString
  go _ acc [] bs = acc <> bs
  go off acc (r : rs) bs =
    let
      sIdx = replaceIndex r - off
      rLen = replacedLength r
      repl = replacedBy r

      (before, after) = second (BS8.drop rLen) $ BS8.splitAt sIdx bs

      acc' = acc <> before <> repl
      off' = off + BS8.length before + replacedLength r
    in
      go off' acc' rs after

replaceOverlaps :: Replace -> Replace -> Bool
replaceOverlaps a b = case (Rampart.relate `on` toInterval) a b of
  Rampart.Overlaps {} -> True
  _ -> False
 where
  toInterval :: Replace -> Rampart.Interval Int
  toInterval r =
    Rampart.toInterval
      (replaceIndex r, replaceIndex r + replacedLength r - 1)

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) pure

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p x = x <$ guard (p x)
