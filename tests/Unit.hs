module Main where

import Parse
import Test.HUnit
import Control.Monad (unless)
import System.Exit (exitFailure)


-- | helper function to read source code from given FilePath and convert
-- it to our IR
--
toIRProcs :: (String -> [a]) -> FilePath -> IO [a]
toIRProcs f p = do
    contents <- readFile p
    return $ f contents

testConvertsToIRSuccessfully :: Test
testConvertsToIRSuccessfully = TestCase $ do
    procs <- toIRProcs convert "tests/data/binsearch.py"
    assertBool "non-empty list expected" (not $ null procs)

tests :: Test
tests = TestList [
    TestLabel "converts source code successfully" testConvertsToIRSuccessfully
    ]

main :: IO ()
main = runTestTT tests >>= \(Counts _ _ e f) ->
        unless (e + f == 0) exitFailure
