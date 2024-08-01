{-# LANGUAGE CPP #-}

module Data.Aeson.Compat.KeyMap
  ( KeyMap
  , union
  , keys
  , insert
  , member
  , lookup
  ) where

#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.KeyMap
import Data.HashMap.Strict ()
#else
import Data.HashMap.Strict
import Data.Text (Text)

type KeyMap = HashMap Text
#endif
