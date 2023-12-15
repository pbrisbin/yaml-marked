module Data.Yaml.Marked.DecodeSpec
  ( spec
  ) where

import Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Yaml.Marked
import Data.Yaml.Marked.Decode
import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Value
import Test.Hspec

data StackYaml = StackYaml
  { resolver :: Marked Text
  , extraDeps :: Marked [Marked Text]
  }
  deriving stock (Eq, Show)

decodeStackYaml :: Marked Value -> Either String (Marked StackYaml)
decodeStackYaml = withObject $ \o ->
  StackYaml
    <$> (text =<< (o .: "resolver"))
    <*> (array text =<< (o .: "extra-deps"))

spec :: Spec
spec = do
  describe "decodeThrow" $ do
    it "decodes with marks" $ do
      let exampleYaml =
            mconcat
              [ "resolver: lts-20.11\n"
              , "extra-deps:\n"
              , " - ../local-package\n"
              , " - hackage-dep-1.0\n"
              ]

      StackYaml {..} <- getMarkedItem <$> decodeThrow decodeStackYaml exampleYaml

      extractMarked resolver exampleYaml `shouldBe` "lts-20.11"

      extractMarked extraDeps exampleYaml
        `shouldBe` "- ../local-package\n - hackage-dep-1.0\n"

      map (`extractMarked` exampleYaml) (getMarkedItem extraDeps)
        `shouldBe` [ "../local-package"
                   , "hackage-dep-1.0"
                   ]

extractMarked :: Marked a -> ByteString -> ByteString
extractMarked m = BS.take (end - start) . BS.drop start
 where
  start = yamlIndex $ getMarkedStart m
  end = yamlIndex $ getMarkedEnd m
