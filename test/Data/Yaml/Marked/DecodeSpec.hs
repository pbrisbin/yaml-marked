module Data.Yaml.Marked.DecodeSpec
  ( spec
  ) where

import Prelude

import Data.Yaml.Marked
import Data.Yaml.Marked.Decode
import Data.Yaml.Marked.Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeThrow" $ do
    it "decodes with marks" $ do
      let
        exampleYaml =
          mconcat
            [ "resolver: lts-20.11\n"
            , "extra-deps:\n"
            , " - ../local-package\n"
            , " - hackage-dep-1.0\n"
            ]

        decodeExample = withObject $ \o ->
          (,)
            <$> (text =<< (o .: "resolver"))
            <*> (array text =<< (o .: "extra-deps"))

      (resolver, extraDeps) <-
        getMarkedItem <$> decodeThrow decodeExample exampleYaml

      getMarkedItem resolver `shouldBe` "lts-20.11"
      getMarkedIndexes resolver `shouldBe` (10, 19)
      getMarkedLength resolver `shouldBe` 9
      getMarkedStartLine resolver `shouldBe` 0
      getMarkedStartColumn resolver `shouldBe` 10
      getMarkedEndLine resolver `shouldBe` 0
      getMarkedEndColumn resolver `shouldBe` 19

      case getMarkedItem extraDeps of
        [extraDep0, extraDep1] -> do
          getMarkedItem extraDep0 `shouldBe` "../local-package"
          getMarkedIndexes extraDep0 `shouldBe` (35, 51)
          getMarkedLength extraDep0 `shouldBe` 16
          getMarkedStartLine extraDep0 `shouldBe` 2
          getMarkedStartColumn extraDep0 `shouldBe` 3
          getMarkedEndLine extraDep0 `shouldBe` 2
          getMarkedEndColumn extraDep0 `shouldBe` 19

          getMarkedItem extraDep1 `shouldBe` "hackage-dep-1.0"
          getMarkedIndexes extraDep1 `shouldBe` (55, 70)
          getMarkedLength extraDep1 `shouldBe` 15
          getMarkedStartLine extraDep1 `shouldBe` 3
          getMarkedStartColumn extraDep1 `shouldBe` 3
          getMarkedEndLine extraDep1 `shouldBe` 3
          getMarkedEndColumn extraDep1 `shouldBe` 18
        _ -> expectationFailure ""
