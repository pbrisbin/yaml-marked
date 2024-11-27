module Data.Yaml.Marked.DecodeSpec
  ( spec
  ) where

import Prelude

import Data.Aeson.Types (JSONPathElement (..))
import Data.Functor.Alt ((<!>))
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Yaml.Marked
import Data.Yaml.Marked.Decode
import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Value
import Test.Hspec

data StackYaml = StackYaml
  { resolver :: Marked Text
  , extraDeps :: Marked [Marked ExtraDep]
  , someSetting :: Marked Bool
  }
  deriving stock (Eq, Show)

decodeStackYaml :: Marked Value -> Either String (Marked StackYaml)
decodeStackYaml = withObject "example" $ \o ->
  StackYaml
    <$> (text =<< (o .: "resolver"))
    <*> (array decodeExtraDep =<< (o .: "extra-deps"))
    <*> (bool =<< (o .: "some-setting"))

data ExtraDep
  = Plain Text
  | Git GitCommit
  deriving stock (Eq, Show)

decodeExtraDep :: Marked Value -> Either String (Marked ExtraDep)
decodeExtraDep x = (fmap Plain <$> text x) <!> (fmap Git <$> decodeGitCommit x)

data GitCommit = GitCommit
  { git :: Marked Text
  , commit :: Marked Text
  }
  deriving stock (Eq, Show)

decodeGitCommit :: Marked Value -> Either String (Marked GitCommit)
decodeGitCommit = withObject "GitCommit" $ \o ->
  GitCommit
    <$> (text =<< o .: "git")
    <*> (text =<< o .: "commit")

spec :: Spec
spec = do
  describe "decodeThrow" $ do
    it "decodes with marks" $ do
      let exampleYaml =
            mconcat
              [ "resolver: lts-20.11\n"
              , "extra-deps:\n"
              , " # A path\n"
              , " - ../local-package\n"
              , "\n"
              , " # A hackage package\n"
              , " - hackage-dep-1.0\n"
              , "\n"
              , " # A git ref\n"
              , " - git: foo\n"
              , "   commit: abc\n"
              , "\n"
              , "# Trailing\n"
              , " - foo-0.0.0\n"
              , "\n"
              , "some-setting: Yes\n"
              , "\n"
              , "# Trailing comment"
              ]

      decodeThrow decodeStackYaml "<input>" exampleYaml
        `shouldReturn` Marked
          { markedItem =
              StackYaml
                { resolver =
                    Marked
                      { markedItem = "lts-20.11"
                      , markedPath = "<input>"
                      , markedJSONPath = Just [Key "resolver"]
                      , markedLocationStart = Location 10 0 10
                      , markedLocationEnd = Location 19 0 19
                      }
                , extraDeps =
                    Marked
                      { markedItem =
                          [ Marked
                              { markedItem = Plain "../local-package"
                              , markedPath = "<input>"
                              , markedJSONPath = Just [Key "extra-deps", Index 0]
                              , markedLocationStart = Location 45 3 3
                              , markedLocationEnd = Location 61 3 19
                              }
                          , Marked
                              { markedItem = Plain "hackage-dep-1.0"
                              , markedPath = "<input>"
                              , markedJSONPath = Just [Key "extra-deps", Index 1]
                              , markedLocationStart = Location 87 6 3
                              , markedLocationEnd = Location 102 6 18
                              }
                          , Marked
                              { markedItem =
                                  Git $
                                    GitCommit
                                      { git =
                                          Marked
                                            { markedItem = "foo"
                                            , markedPath = "<input>"
                                            , markedJSONPath = Just [Key "extra-deps", Index 2, Key "git"]
                                            , markedLocationStart = Location 125 9 8
                                            , markedLocationEnd = Location 128 9 11
                                            }
                                      , commit =
                                          Marked
                                            { markedItem = "abc"
                                            , markedPath = "<input>"
                                            , markedJSONPath = Just [Key "extra-deps", Index 2, Key "commit"]
                                            , markedLocationStart = Location 140 10 11
                                            , markedLocationEnd = Location 143 10 14
                                            }
                                      }
                              , markedPath = "<input>"
                              , markedJSONPath = Just [Key "extra-deps", Index 2]
                              , markedLocationStart = Location 120 9 3
                              , markedLocationEnd = Location 143 10 14
                              }
                          , Marked
                              { markedItem = Plain "foo-0.0.0"
                              , markedPath = "<input>"
                              , markedJSONPath = Just [Key "extra-deps", Index 3]
                              , markedLocationStart = Location 159 13 3
                              , markedLocationEnd = Location 168 13 12
                              }
                          ]
                      , markedPath = "<input>"
                      , markedJSONPath = Just [Key "extra-deps"]
                      , markedLocationStart = Location 43 3 1
                      , markedLocationEnd = Location 168 13 12
                      }
                , someSetting =
                    Marked
                      { markedItem = True
                      , markedPath = "<input>"
                      , markedJSONPath = Just [Key "some-setting"]
                      , markedLocationStart = Location 184 15 14
                      , markedLocationEnd = Location 187 15 17
                      }
                }
          , markedPath = "<input>"
          , markedJSONPath = Just []
          , markedLocationStart = Location 0 0 0
          , markedLocationEnd = Location 187 15 17
          }

    context "errors" $ do
      it "incorrect type" $ do
        let exampleYaml =
              mconcat
                [ "resolver: lts-20.11\n"
                , "extra-deps:\n"
                , "  - Yes\n" -- expected String, encountered Boolean
                ]

        decodeThrow decodeStackYaml "<input>" exampleYaml
          `shouldThrow` aesonExceptionPrefixed "Error in $['extra-deps'][0]: "

      it "missing key" $ do
        let exampleYaml =
              mconcat
                [ "extra-deps:\n"
                , "  - foo-1.0.1\n"
                ]

        decodeThrow decodeStackYaml "<input>" exampleYaml
          `shouldThrow` aesonExceptionPrefixed "Error in $: Key not found"

      it "missing nested key" $ do
        let exampleYaml =
              mconcat
                [ "resolver: lts-20.11\n"
                , "extra-deps:\n"
                , "  - foo-1.0.1\n"
                , "  - git: abc\n" -- no commit
                ]

        decodeThrow decodeStackYaml "<input>" exampleYaml
          `shouldThrow` aesonExceptionPrefixed "Error in $['extra-deps'][1]: Key not found: commit"

aesonExceptionPrefixed :: String -> ParseException -> Bool
aesonExceptionPrefixed x = \case
  AesonException msg -> x `isPrefixOf` msg
  _ -> False
