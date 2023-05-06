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
-----------------------------------------------------------------------------
import Data.Proxy   ( Proxy(Proxy)
                    )
import GHC.TypeLits ( KnownSymbol
                    , Symbol, UnconsSymbol
                    , KnownChar, charVal
                    )
import Numeric      ( showEFloat
                    , showHex
                    , showOct
                    )
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
printf :: forall symbol input . Printf symbol input => Arg input
printf = interpC (Proxy @input) id
-----------------------------------------------------------------------------
-- | Class for printing C-style formatting strings
type Printf symbol input =
     ( KnownSymbol symbol
     , input ~ Parse symbol
     , CPrintf input
     )

type family Parse (xs :: Symbol) where
  Parse xs = PostProcess (Lex (UnconsSymbol xs))

type family Lex (xs :: Maybe (Char, Symbol)) where
  Lex 'Nothing              = '[]
  Lex ( 'Just '( '%', xs )) = Percent ': Lex (UnconsSymbol xs)
  Lex ( 'Just '( c, xs ))   = Character c ': Lex (UnconsSymbol xs)

type family PostProcess xs where
  PostProcess '[]                                               = '[]
  PostProcess (Percent ': Character 'd' ': xs)                  = D  ': PostProcess xs
  PostProcess (Percent ': Character 'f' ': xs)                  = F  ': PostProcess xs
  PostProcess (Percent ': Character 's' ': xs)                  = S  ': PostProcess xs
  PostProcess (Percent ': Character 'e' ': xs)                  = E  ': PostProcess xs
  PostProcess (Percent ': Character 'E' ': xs)                  = E  ': PostProcess xs
  PostProcess (Percent ': Character 'c' ': xs)                  = C  ': PostProcess xs
  PostProcess (Percent ': Character 'i' ': xs)                  = I  ': PostProcess xs
  PostProcess (Percent ': Character 'x' ': xs)                  = X  ': PostProcess xs
  PostProcess (Percent ': Character 'o' ': xs)                  = O  ': PostProcess xs
  PostProcess (Percent ': Character 'l' ': Character 'f' ': xs) = LF ': PostProcess xs
  PostProcess (x ': xs)                                         = x  ': PostProcess xs

-- | Class used to Codegen C printing functions from lexed / parsed Symbols.
class CPrintf c where
  type Arg c
  interpC
    :: Proxy c
    -> ShowS
    -> Arg c

-- | Tokens
data Character (c :: Char)

-- | Terms
data Percent
data D
data E
data F
data S
data C
data I
data O
data LF
data X

instance CPrintf '[] where
  type Arg '[]         = String
  interpC Proxy result = result mempty

instance (KnownChar c, CPrintf cs) => CPrintf (Character c ': cs) where
  type Arg (Character c ': cs) = Arg cs
  interpC Proxy xs =
    interpC (Proxy @cs) (xs . x)
      where
        x = showChar (charVal (Proxy @c))

instance CPrintf cs => CPrintf (D : cs) where
  type Arg (D : cs) = Int -> Arg cs
  interpC Proxy xs n =
    interpC (Proxy @cs) (xs . shows n)

instance CPrintf cs => CPrintf (F : cs) where
  type Arg (F : cs) = Float -> Arg cs
  interpC Proxy xs n =
    interpC (Proxy @cs) (xs . shows n)

instance CPrintf cs => CPrintf (LF : cs) where
  type Arg (LF : cs) = Double -> Arg cs
  interpC Proxy xs n =
    interpC (Proxy @cs) (xs . shows n)

instance CPrintf cs => CPrintf (S : cs) where
  type Arg (S : cs) = String -> Arg cs
  interpC Proxy xs n =
    interpC (Proxy @cs) (xs . showString n)

instance CPrintf cs => CPrintf (Percent : cs) where
  type Arg (Percent : cs) = Arg cs
  interpC Proxy xs = interpC (Proxy @cs) (xs . showChar '%')

instance CPrintf cs => CPrintf (C : cs) where
  type Arg (C : cs) = Char -> Arg cs
  interpC Proxy xs c = interpC (Proxy @cs) (xs . showChar c)

instance CPrintf cs => CPrintf (E : cs) where
  type Arg (E : cs) = Double -> Arg cs
  interpC Proxy xs d = interpC (Proxy @cs) (xs . x)
    where
      x = showString (showEFloat Nothing d "")

instance CPrintf cs => CPrintf (X : cs) where
  type Arg (X : cs) = Int -> Arg cs
  interpC Proxy xs d = interpC (Proxy @cs) (xs . x)
    where
      x = showString (showHex d "")

instance CPrintf cs => CPrintf (O : cs) where
  type Arg (O : cs) = Int -> Arg cs
  interpC Proxy xs d = interpC (Proxy @cs) (xs . x)
    where
      x = showString (showOct d "")
