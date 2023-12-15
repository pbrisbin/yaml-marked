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
      `shouldBe` mconcat
        [ "resolver: lts-20.11\n"
        , "extra-deps:\n"
        , " - ../local-package\n"
        , " - hackage-dep-2.0.1\n"
        ]
