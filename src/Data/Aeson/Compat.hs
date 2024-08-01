{-# LANGUAGE CPP #-}

module Data.Aeson.Compat
#if MIN_VERSION_aeson(2, 0, 0)
  ( module Data.Aeson
  ) where

import Data.Aeson
#else
  ( Key
  , module Data.Aeson
  ) where

import Data.Aeson
import Data.Text (Text)

type Key = Text
#endif
