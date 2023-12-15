module Data.Yaml.Marked.Replace
  ( Replace
  , newReplace
  , runReplaces
  ) where

import Prelude

import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml.Marked

data Replace = Replace
  { replaceIndex :: Int
  , replacedLength :: Int
  , replacedBy :: ByteString
  }
  deriving stock (Show)

newReplace :: Marked a -> ByteString -> Replace
newReplace m bs =
  Replace
    { replaceIndex = getMarkedStartIndex m
    , replacedLength = getMarkedEndIndex m - getMarkedStartIndex m
    , replacedBy = bs
    }

runReplaces :: [Replace] -> ByteString -> ByteString
runReplaces = go 0 ""
 where
  go :: Int -> ByteString -> [Replace] -> ByteString -> ByteString
  go _ acc [] bs = acc <> bs
  go off acc (s : ss) bs =
    let
      sIdx = replaceIndex s - off
      rLen = replacedLength s
      repl = replacedBy s

      (before, after) = second (BS8.drop rLen) $ BS8.splitAt sIdx bs

      acc' = acc <> before <> repl
      off' = off + BS8.length before + replacedLength s
    in
      go off' acc' ss after
