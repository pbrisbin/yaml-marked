module Data.Yaml.Marked.InternalSpec
  ( spec
  ) where

import Prelude

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (traverse_)
import qualified Data.Yaml as Yaml
import Data.Yaml.Marked
import Data.Yaml.Marked.Decode
import Data.Yaml.Marked.Parse
import Test.Hspec
import Test.Hspec.Expectations.Json

spec :: Spec
spec = do
  context "matching Data.Yaml.decode" $ do
    decodeTestCases
      [ "x: 1e-100000"
      , "x: 9.78159610558926e-5"
      , "12345"
      , "+12345"
      , "0o14"
      , "0o123"
      , "0xC"
      , "0xc"
      , "0xdeadBEEF"
      , "0xDEADBEEF"
      , "1.23015e+3"
      , "12.3015e+02"
      , "1230.15"
      , "---\na:\n  &id5 value: 1.0\nb:\n  *id5: 1.2"
      , "foo:\n  - &anchor bin1\n  - bin2\n  - bin3"
      , "foo: &anchor\n  - bin1\n  - bin2\n  - bin3"
      , "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3"
      , "foo: &anchor\n  key1: bin1\n  key2: bin2\n  key3: bin3\nboo: *anchor"
      , "foo: !bar\n  k: v\n  k2: v2"
      , "foo: !\n  k: v\n  k2: v2"
      , "foo: !bar [x, y, z]"
      , "foo: ! [x, y, z]"
      , "foo: [x, y, z]"
      , "foo:\n- x\n- y\n- z\n"
      , "foo: { bar: 1, baz: 2 }"
      , "foo: bar\nbaz: quux"
      , "foo:\n  baz: [bin1, bin2, bin3]\nbaz: bazval"
      , "m1: &m1\n  k1: !!str 1\n  k2: !!str 2\nm2: &m2\n  k1: !!str 3\n  k3: !!str 4\nfoo1: foo\n<<: [ *m1, *m2 ]"
      , "- &anch foo\n- baz\n- *anch"
      , "seq: &anch\n  - foo\n  - baz\nseq2: *anch"
      , "map: &anch\n  key1: foo\n  key2: baz\nmap2: *anch"
      , "- &anch foo\n- baz\n- *anch\n- &anch boo\n- buz\n- *anch"
      , "foo1: foo\nfoo2: baz\nfoo1: buz"
      , "foo1: foo\nfoo2: baz\n<<:\n  foo1: buz\n  foo3: fuz"
      , "m1: &m1\n  k1: !!str 1\n  k2: !!str 2\nm2: &m2\n  k1: !!str 3\n  k3: !!str 4\nfoo1: foo\n<<: [ *m1, *m2 ]"
      , "foo: \"1234\""
      , "foo: 1234"
      , "foo: !!str 1234"
      , "1\n"
      , "foo: off\nbar: y\nbaz: true"
      , "foo: FALSE\nbar: Y\nbaz: ON"
      , "foo: No\nbar: Yes\nbaz: True"
      , "Default: &def\n  foo: 1\n  bar: 2\nObj:\n  <<: *def\n  key: 3\n"
      , "null"
      , "Null"
      , "NULL"
      , "~"
      , ""
      , "# comment\n"
      ]

  context "matching Data.Yaml.decodeAll" $ do
    decodeAllTestCases
      [ ""
      , "# foo\n# bar"
      , "foo: true"
      , "--- 1\n--- 2"
      ]

  context "decode failing" $ do
    decodeFailTestCases
      [ "  - foo\n  - baz\nbuz"
      , "\tthis is 'not' valid :-)"
      , "map: *anch\nmap2: &anch\n  key1: foo\n  key2: baz"
      , "map: &anch\n  key1: foo\n  key2: *anch"
      ]

decodeTestCases :: [ByteString] -> Spec
decodeTestCases = traverse_ $ \yaml -> do
  it ("for " <> toDocStringYaml yaml) $ do
    expected <- Yaml.decodeThrow yaml
    actual <- decodeThrow value yaml
    expected `shouldBeJson` getMarkedItem actual

decodeAllTestCases :: [ByteString] -> Spec
decodeAllTestCases = traverse_ $ \yaml -> do
  it ("for " <> toDocStringYaml yaml) $ do
    expected <- Yaml.decodeAllThrow @_ @Aeson.Value yaml
    actual <- decodeAllThrow value yaml
    Aeson.toJSON expected `shouldBeJson` Aeson.toJSON (map getMarkedItem actual)

decodeFailTestCases :: [ByteString] -> Spec
decodeFailTestCases = traverse_ $ \yaml -> do
  it ("for " <> toDocStringYaml yaml) $ do
    Yaml.decodeThrow @Maybe @Aeson.Value yaml `shouldBe` Nothing
    decodeThrow value yaml `shouldBe` Nothing

toDocStringYaml :: ByteString -> String
toDocStringYaml yaml = show truncated
 where
  truncated
    | BS8.length yaml > limit = (<> "...") $ BS8.take (limit - 3) yaml
    | otherwise = yaml

  limit = 50
