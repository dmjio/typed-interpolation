{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Interpolation
-- Copyright   :  (C) 2023 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Interpolation (printf) where

import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits ( KnownSymbol
                    , Symbol, UnconsSymbol
                    , KnownChar, charVal
                    )

-- | 'printf'
-- C-style print formatting
--
-- > data Person
-- >  = Person
-- >  { name :: String
-- >  , age :: Int
-- >  } deriving Eq
--
-- > instance Show Person where
-- >  show (Person name age) = printf @"My name is %s, I am %d years old" name age
--
printf :: forall symbol input . Printf symbol input => Arg input
printf = interpC (Proxy @input) mempty

-- | Class for printing C-style formatting strings
type Printf symbol input =
     ( KnownSymbol symbol
     , input ~ Tokenize symbol
     , C input
     )

type family Tokenize (xs :: Symbol) where
  Tokenize xs = Pad (Lex (UnconsSymbol xs))

type family Lex (xs :: Maybe (Char, Symbol)) where
  Lex 'Nothing              = '[]
  Lex ( 'Just '( '%', xs )) = Percent ': Lex (UnconsSymbol xs)
  Lex ( 'Just '( c, xs ))   = Character c ': Lex (UnconsSymbol xs)

type family Pad xs where
  Pad '[]                              = '[]
  Pad (Percent ': Character 'd' ': xs) = D ': Pad xs
  Pad (Percent ': Character 'f' ': xs) = F ': Pad xs
  Pad (Percent ': Character 's' ': xs) = S ': Pad xs
  Pad (x ': xs)                        = x ': Pad xs

class C c where
  type Arg c
  interpC
    :: Proxy c
    -> String
    -> Arg c

data Character (c :: Char)
data Percent
data D
data F
data S

instance C '[] where
  type Arg '[]         = String
  interpC Proxy result = reverse result

instance (KnownChar c, C cs) => C (Character c ': cs) where
  type Arg (Character c ': cs) = Arg cs
  interpC Proxy xs =
    interpC (Proxy @cs) (x:xs)
      where
        x = charVal (Proxy @c)

instance C cs => C (D : cs) where
  type Arg (D : cs) = Int -> Arg cs
  interpC Proxy xs n =
    interpC (Proxy @cs) (reverse (show n) <> xs)

instance C cs => C (F : cs) where
  type Arg (F : cs) = Double -> Arg cs
  interpC Proxy xs n =
    interpC (Proxy @cs) (reverse (show n) <> xs)

instance C cs => C (S : cs) where
  type Arg (S : cs) = String -> Arg cs
  interpC Proxy xs n =
    interpC (Proxy @cs) (reverse n <> xs)

instance C cs => C (Percent : cs) where
  type Arg (Percent : cs) = Arg cs
  interpC Proxy xs = interpC (Proxy @cs) ('%' : xs)
