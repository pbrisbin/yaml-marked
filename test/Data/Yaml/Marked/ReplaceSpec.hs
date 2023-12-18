module Data.Yaml.Marked.ReplaceSpec
  ( spec
  ) where

import Prelude

import Data.Yaml.Marked
import Data.Yaml.Marked.Decode
import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Replace
import Test.Hspec

spec :: Spec
spec = do
  describe "runReplaces" $ do
    it "works for a replacement at start" $ do
      let r = newReplace 0 4 "that"
      runReplaces [r] "this text" `shouldReturn` "that text"

    it "works for replacement at start and end" $ do
      let rs =
            [ newReplace 0 4 "that"
            , newReplace 13 3 "cold"
            ]

      runReplaces rs "this text is hot" `shouldReturn` "that text is cold"

    it "works for replacement at start, middle, and end" $ do
      let rs =
            [ newReplace 0 4 "that"
            , newReplace 18 3 "cold"
            , newReplace 10 4 "won't"
            ]

      runReplaces rs "this text will be hot"
        `shouldReturn` "that text won't be cold"

    context "validations" $ do
      it "NegativeStartIndex" $ do
        let r = newReplace (negate 1) 0 ""
        runReplaces [r] "" `shouldThrow` (== NegativeStartIndex r)

      it "NegativeLength" $ do
        let r = newReplace 0 (negate 1) ""
        runReplaces [r] "" `shouldThrow` (== NegativeLength r)

      it "ReplaceOutOfBounds" $ do
        let r = newReplace 0 3 ""
        runReplaces [r] "x" `shouldThrow` (== ReplaceOutOfBounds r 1)

      it "ReplaceOutOfBounds by start" $ do
        let r = newReplace 3 3 ""
        runReplaces [r] "x" `shouldThrow` (== ReplaceOutOfBounds r 1)

      it "ReplaceOutOfBounds with multiple" $ do
        let
          r = newReplace 7 3 ""
          rs =
            [ newReplace 0 2 ""
            , newReplace 3 3 ""
            , r
            ]

        runReplaces rs "123456" `shouldThrow` (== ReplaceOutOfBounds r 0)

      it "OverlappingReplace" $ do
        let
          overlapped = newReplace 2 3 ""
          overlapping = newReplace 3 3 ""
          rs =
            [ newReplace 0 1 ""
            , overlapping
            , newReplace 9 1 ""
            , overlapped
            , newReplace 12 1 ""
            ]

        runReplaces rs "123456789abcdefhigjkl"
          `shouldThrow` (== OverlappingReplace overlapping)

  it "works for a realistic stack.yaml example" $ do
    let
      exampleYaml =
        mconcat
          [ "resolver: lts-20.0\n"
          , "extra-deps:\n"
          , " - ../local-package\n"
          , " - hackage-dep-1.0\n"
          ]

      decodeExample = withObject "example" $ \o ->
        (,)
          <$> (text =<< (o .: "resolver"))
          <*> (array text =<< (o .: "extra-deps"))

    (resolver, extraDeps) <-
      getMarkedItem <$> decodeThrow decodeExample "<input>" exampleYaml

    let replaces =
          [ replaceMarked resolver "lts-20.11"
          , replaceMarked (getMarkedItem extraDeps !! 1) "hackage-dep-2.0.1"
          ]

    runReplaces replaces exampleYaml
      `shouldReturn` mconcat
        [ "resolver: lts-20.11\n"
        , "extra-deps:\n"
        , " - ../local-package\n"
        , " - hackage-dep-2.0.1\n"
        ]
