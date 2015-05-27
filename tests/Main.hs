module Main where

import Test.HUnit
import Unit(tests)
import Control.Monad (unless)
import System.Exit (exitFailure)

main :: IO ()
main = runTestTT tests >>= \(Counts _ _ e f) ->
        unless (e + f == 0) exitFailure
