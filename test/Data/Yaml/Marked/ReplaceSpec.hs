module Data.Yaml.Marked.ReplaceSpec
  ( spec
  ) where

import Prelude

import Data.Text (Text)
import Data.Yaml.Marked
import Data.Yaml.Marked.Decode
import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Replace
import Data.Yaml.Marked.Value
import Test.Hspec

data StackYaml = StackYaml
  { resolver :: Marked Text
  , extraDeps :: Marked [Marked Text]
  }

decodeStackYaml :: Marked Value -> Either String (Marked StackYaml)
decodeStackYaml = withObject $ \o ->
  StackYaml
    <$> (text =<< (o .: "resolver"))
    <*> (array text =<< (o .: "extra-deps"))

spec :: Spec
spec = do
  describe "runReplaces" $ do
    it "makes multiple replacements that change size" $ do
      let exampleYaml =
            mconcat
              [ "resolver: lts-20.0\n"
              , "extra-deps:\n"
              , " - ../local-package\n"
              , " - hackage-dep-1.0\n"
              ]

      stackYaml <- decodeThrow decodeStackYaml exampleYaml

      let
        mResolver = resolver $ getMarkedItem stackYaml
        mExtraDep = getMarkedItem (extraDeps $ getMarkedItem stackYaml) !! 1
        replaces =
          [ newReplace mResolver "lts-20.11"
          , newReplace mExtraDep "hackage-dep-2.0.1"
          ]

      runReplaces replaces exampleYaml
        `shouldReturn` mconcat
          [ "resolver: lts-20.11\n"
          , "extra-deps:\n"
          , " - ../local-package\n"
          , " - hackage-dep-2.0.1\n"
          ]

  context "validations" $ do
    let
      someMarked :: Int -> Int -> Marked Text
      someMarked a b =
        markedItem "this text" (YamlMark a 0 0) (YamlMark b 0 0)

      someReplaceAt :: Int -> Int -> Replace
      someReplaceAt a b = newReplace (someMarked a b) "that text"

    it "NegativeStartIndex" $ do
      let r = someReplaceAt (negate 1) 0
      runReplaces [r] "" `shouldThrow` (== NegativeStartIndex r)

    it "NegativeLength" $ do
      let r = someReplaceAt 0 (negate 1)
      runReplaces [r] "" `shouldThrow` (== NegativeLength r)

    it "ReplaceOutOfBounds" $ do
      let r = someReplaceAt 0 2
      runReplaces [r] "x" `shouldThrow` (== ReplaceOutOfBounds r 1)

    it "ReplaceOutOfBounds by start" $ do
      let r = someReplaceAt 3 5
      runReplaces [r] "x" `shouldThrow` (== ReplaceOutOfBounds r 1)

    it "ReplaceOutOfBounds with multiple" $ do
      let
        r = someReplaceAt 7 9
        rs =
          [ someReplaceAt 0 1
          , someReplaceAt 3 5
          , r
          ]

      runReplaces rs "123456" `shouldThrow` (== ReplaceOutOfBounds r 1)

    it "OverlappingReplace" $ do
      let
        overlapped = someReplaceAt 2 4
        overlapping = someReplaceAt 3 5
        rs =
          [ someReplaceAt 0 2
          , overlapping
          , someReplaceAt 9 10
          , overlapped
          , someReplaceAt 12 13
          ]

      runReplaces rs "123456789abcdefhigjkl"
        `shouldThrow` (== OverlappingReplace overlapping)
