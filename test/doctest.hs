module Main where

import Test.DocTest

main :: IO ()
main = doctest [
       "-XKindSignatures",
       "-XDataKinds",
       "-idist/build",
       "Data/Fin.hs"
       ]

