module Main where

import Test.DocTest

main :: IO ()
main = doctest [  "-isrc"
                , "src/Main.hs"
                , "src/Programslice/Visualisation.hs"
                ]
