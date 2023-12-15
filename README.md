## yaml-marked

[![Hackage](https://img.shields.io/hackage/v/yaml-marked.svg?style=flat)](https://hackage.haskell.org/package/yaml-marked)
[![Stackage Nightly](http://stackage.org/package/yaml-marked/badge/nightly)](http://stackage.org/nightly/package/yaml-marked)
[![Stackage LTS](http://stackage.org/package/yaml-marked/badge/lts)](http://stackage.org/lts/package/yaml-marked)
[![CI](https://github.com/pbrisbin/yaml-marked/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/yaml-marked/actions/workflows/ci.yml)

## Motivation

While working on [`stack-lint-extra-deps`][sled], it became apparent it would be
impossible to implement an auto-fix in a way that preserved the source
formatting of the original Yaml. Sure, we could decode, modify, and encode, but
that loses all formatting, comments, and sorting -- which can be important for
documenting choices made in one's `stack.yaml`.

[sled]: https://github.com/freckle/stack-lint-extra-deps

That is, unless there were a Yaml parser that preserved source locations. Then,
I could use those to make targeted replacements within the original source
`ByteString`, while preserving everything else.

There didn't seem to be anything promising on Hackage, except for the fact that
`Text.Libyaml` (the library powering `Data.Yaml`) has functions that emit
`MarkedEvent`s, which `Data.Yaml` doesn't use.

So I began the process of modifying `Data.Yaml` internals to operate on
`MarkedEvent`s and, instead of assuming `FromJSON` to construct a `Value`, use a
provided decoding function and produce my own `Marked Value` type -- which is
basically the same, except holding the original `YamlMark` (recursively). Thus,
`Data.Yaml.Marked.Decode` was born.

Finally, I created something to approximate a non-typeclass `FromJSON`
(`Data.Yaml.Marked.Parse`), and a module to handle the tricky business of
applying replacements to `ByteString`s (`Data.Yaml.Marked.Replace`), and I can
finally do what I need to:

```hs
-- Kind of like you'd do a record for FromJSON, except with Marked wrapping
-- wherever you need it. Notice the extraDeps list itself is Marked, as is each
-- element.
data StackYaml = StackYaml
  { resolver :: Marked Text
  , extraDeps :: Marked [Marked Text]
  }

-- Data.Yaml.Marked.Parse exposes Aeson-inspired functions for building a
-- decoding function
decodeStackYaml :: Marked Value -> Either String (Marked StackYaml)
decodeStackYaml = withObject "StackYaml" $ \o ->
  StackYaml
    <$> (text =<< (o .: "resolver"))
    <*> (array text =<< (o .: "extra-deps"))

main :: IO ()
main = do
  stackYaml <- BS8.readFile "stack.yaml"
  -- Imagine:
  --
  --   resolver: lts-20.0
  --   extra-deps:
  --    - ../local-package
  --    - hackage-dep-1.0
  --

  -- We don't need the location values that represent the entire file, so we can
  -- discard them here.
  StackYaml {..} <- getMarkedItem <$> decodeThrow decodeStackYaml stackYaml

  let replaces =
        -- Pretend we identified the resolver and second extra-dep as old and
        -- built Replace values for each in the process of doing so.
        [ replaceMarked resolver "lts-20.11"
        , replaceMarked (getMarkedItem extraDeps !! 1) "hackage-dep-2.0.1"
        ]

  BS8.putStr =<< runReplaces replaces stackYaml
  --
  -- Outputs:
  --
  --   resolver: lts-20.11
  --   extra-deps:
  --    - ../local-package
  --    - hackage-dep-2.0.1
  --
```

## Development & Tests

```console
stack build --fast --pedantic --test --file-watch
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
