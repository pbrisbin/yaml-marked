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

    it "works for adjacent replaces" $ do
      let rs =
            [ newReplace 0 4 ""
            , newReplace 4 4 ""
            ]

      runReplaces rs "012\n012\n012\n" `shouldReturn` "012\n"

    context "validations" $ do
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
        markedItem <$> decodeThrow decodeExample "<input>" exampleYaml

      let replaces =
            [ replaceMarked resolver "lts-20.11"
            , replaceMarked (markedItem extraDeps !! 1) "hackage-dep-2.0.1"
            ]

      runReplaces replaces exampleYaml
        `shouldReturn` mconcat
          [ "resolver: lts-20.11\n"
          , "extra-deps:\n"
          , " - ../local-package\n"
          , " - hackage-dep-2.0.1\n"
          ]

  describe "runReplacesOnOverlapping" $ do
    it "can skip overlapping replaces" $ do
      let
        exampleYaml =
          mconcat
            [ "resolver: lts-18.18\n"
            , "\n"
            , "packages:\n"
            , "  # Missing in Stackage, newer in Hackage\n"
            , "  - freckle-app-1.0.1.1\n"
            , "\n"
            , "  # Exists in resolver now, newer in Hackage too\n"
            , "  - dhall-1.37.1@sha256:447031286e8fe270b0baacd9cc5a8af340d2ae94bb53b85807bee93381ca5287,35080\n"
            , "  - generic-lens-2.0.0.0@sha256:7409fa0ce540d0bd41acf596edd1c5d0c0ab1cd1294d514cf19c5c24e8ef2550,3866\n"
            , "\n"
            , "  # Newer in Hackage\n"
            , "  - faktory-1.1.2.0@sha256:b2a1986095aefa645f6ad701504e1671ddfeb36f11b68434837fc9fcd3594ca8,9078\n"
            , "  - hspec-core-2.8.3@sha256:e4c1e08e16842804c470c2b4722e417ed2a556a47a74107401ad42195522f7a7,4992\n"
            , "  - hspec-2.8.3@sha256:43a42e23994a6c61a7adead7e8a412016680234f5a14a157f68c020871e59534,1709\n"
            , "  - hspec-junit-formatter-1.0.1.0@sha256:6429871263be4532a3a6d08e72da5a8e4c00e8bfbfd7fc5b8352a3643f867453,2652\n"
            , "\n"
            , "  # New version available\n"
            , "  - network-3.1.2.5@sha256:433a5e076aaa8eb3e4158abae78fb409c6bd754e9af99bc2e87583d2bcd8404a,4888\n"
            , "\n"
            , "  # Newer commits, version-like tag exists too\n"
            , "  - github: freckle/yesod-routes-flow\n"
            , "    commit: 2a9cd873880956dd9a0999b593022d3c746324e8\n"
            , "\n"
            , "  # No suggestions (yet)\n"
            , "  - git: https://github.com/freckle/asana\n"
            , "    commit: 91ce6ade118674bb273966943370684eba71f227\n"
            ]

        replaces =
          [ newReplace 77 19 "freckle-app-1.12.0.0"
          , newReplace 151 90 "dhall-1.42.1"
          , newReplace 246 97 "generic-lens-2.2.2.0"
          , newReplace 370 92 "faktory-1.1.2.5"
          , newReplace 467 93 "hspec-core-2.11.7"
          , newReplace 565 88 "hspec-2.11.7"
          , newReplace 658 106 "hspec-junit-formatter-1.1.0.2"
          , newReplace 796 92 "network-3.1.4.0"
          , newReplace 941 86 "yesod-routes-flow-3.0.0.2"
          , newReplace 987 40 "94f9343305ef98d12026ab1f96460dd8b6d29f6f" -- < skipped due to overlap
          , newReplace 1108 40 "e9d43d953957efc06a0d20bef786a959d03198e6"
          ]

      runReplacesOnOverlapping (const $ pure ()) replaces exampleYaml
        `shouldReturn` mconcat
          [ "resolver: lts-18.18\n"
          , "\n"
          , "packages:\n"
          , "  # Missing in Stackage, newer in Hackage\n"
          , "  - freckle-app-1.12.0.0\n"
          , "\n"
          , "  # Exists in resolver now, newer in Hackage too\n"
          , "  - dhall-1.42.1\n"
          , "  - generic-lens-2.2.2.0\n"
          , "\n"
          , "  # Newer in Hackage\n"
          , "  - faktory-1.1.2.5\n"
          , "  - hspec-core-2.11.7\n"
          , "  - hspec-2.11.7\n"
          , "  - hspec-junit-formatter-1.1.0.2\n"
          , "\n"
          , "  # New version available\n"
          , "  - network-3.1.4.0\n"
          , "\n"
          , "  # Newer commits, version-like tag exists too\n"
          , "  - yesod-routes-flow-3.0.0.2\n"
          , "\n"
          , "  # No suggestions (yet)\n"
          , "  - git: https://github.com/freckle/asana\n"
          , "    commit: e9d43d953957efc06a0d20bef786a959d03198e6\n"
          ]
