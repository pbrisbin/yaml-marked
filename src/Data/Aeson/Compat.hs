{-# LANGUAGE CPP #-}

module Data.Aeson.Compat
#if MIN_VERSION_aeson(2, 0, 0)
  ( module Data.Aeson
  , parseEither
  ) where

import Data.Aeson

#if MIN_VERSION_aeson(2, 1, 0)
import Prelude
import Data.Aeson.Types (Parser, IResult(..), iparse)

parseEither :: (a -> Parser b) -> a -> Either String b
parseEither f a = case iparse f a of
  IError _ x -> Left x
  ISuccess b -> Right b
#else
import Data.Aeson.Types (parseEither)
#endif

#else
  ( Key
  , module Data.Aeson
  , parseEither
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Aeson.Types (parseEither)

type Key = Text
#endif
