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
