# typed-interpolation

Generate formatted strings in a type-safe way

```haskell
{-# LANGUAGE TypeApplications, DataKinds #-}
module Main where

import Interpolation (printf)

data Person
  = Person
  { name :: String
  , age :: Int
  } deriving Eq

instance Show Person where
  show (Person name age) =
    let
      printPerson :: String -> Int -> String
      printPerson = printf @"My name is %s, I am %d years old"
    in
      printPerson name age

main :: IO ()
main = print (Person "jimmy" 99)

-- My name is jimmy, I am 99 years old
```

# Build

```bash
cabal build
```

# Demo

```bash
cabal run
```

# Notes
  - No `template-haskell` required
  - Right now only C-style formatting is supported
  - Roadmap:
    - `HasField` support
    - Other language string formatting syntaxes (e.g. `Nix`, `rust`)