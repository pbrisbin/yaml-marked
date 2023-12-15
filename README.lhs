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

## Example

<!--
```haskell
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main (main) where
import Prelude
import Text.Markdown.Unlit ()
```
-->

### Imports

```haskell
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml.Marked
import Data.Yaml.Marked.Value
import Data.Yaml.Marked.Parse
import Data.Yaml.Marked.Decode
import Data.Yaml.Marked.Replace
```

### Decoding

Decoding is mean to look a lot like aeson. Your record should hold `Marked`
values anywhere you will need that:

```haskell
data StackYaml = StackYaml
  { resolver :: Marked Text
  , extraDeps :: Marked [Marked Text]
  }
```

Instead of making a `FromJSON` instance, you just define a function. The
`Data.Yaml.Marked.Parse` module exposes combinators to accomplish this:

```haskell
decodeStackYaml :: Marked Value -> Either String (Marked StackYaml)
decodeStackYaml = withObject $ \o ->
  StackYaml
    <$> (text =<< (o .: "resolver"))
    <*> (array text =<< (o .: "extra-deps"))
```

Then we can give this to `Data.Yaml.Marked.Decode.decodeThrow`:

```haskell
example :: IO ()
example = do
  stackYaml <- BS8.readFile "stack.yaml"
  -- Imagine:
  --
  --   resolver: lts-20.0
  --   extra-deps:
  --    - ../local-package
  --    - hackage-dep-1.0
  --

  StackYaml {..} <- getMarkedItem <$> decodeThrow decodeStackYaml stackYaml
```

Because our decoder returns a `Marked StackYaml`, that's what we get. We don't
need the location info for this (it just represents the entire file), so we
discard it here. We could've written our decoder to return just a `StackYaml`,
but then we could not have used `withObject`, which needs to return `Marked` in
case it's being used somewhere other than the top-level document, where you
would indeed want `Marked` values.

Next, let's pretend we linted this value and discovered the resolver and the
second extra-dep can be updated. As part of doing this we can build a `Replace`
value from the `Marked` item and what it should be updated to.

```haskell
  let replaces =
        [ replaceMarked resolver "lts-20.11"
        , replaceMarked (getMarkedItem extraDeps !! 1) "hackage-dep-2.0.1"
        ]
```

Then we can just runs these on the original `ByteString`:

```haskell
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

<!--
```haskell
main :: IO ()
main = pure ()
```
--->

## Development & Tests

```console
stack build --fast --pedantic --test --file-watch
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
