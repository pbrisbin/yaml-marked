{-# LANGUAGE CPP #-}

module Data.Aeson.Compat.Key
  ( Key
  , fromText
  , toText
  ) where

#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.Key
#else
import Prelude (id)
import Data.Text (Text)

type Key = Text

fromText :: Text -> Key
fromText = id

toText :: Key -> Text
toText = id
#endif
