{-# LANGUAGE GADTs #-}
module Unit where

import Fixtures
import Test.HUnit
import Compiler.Hoopl

import Programslice.Parse
import Programslice.Python.ControlFlow (astToCFG, CFG(..), getInternalGraph)


-- | helper function to read source code from given FilePath and convert
-- it to our IR
--
toIRProcs :: (String -> [a]) -> FilePath -> IO [a]
toIRProcs f p = do
    contents <- readFile p
    return $ f contents

testParsesToCFGSuccessfully :: Test
testParsesToCFGSuccessfully = TestCase $ do
    procs <- toIRProcs parse "tests/data/binsearch.py"
    assertBool "non-empty list expected" (not $ null procs)

testCreatesCFGSuccessfully :: Test
testCreatesCFGSuccessfully = TestCase $
    assertBool "expecting non-empty CFG graph" (isHooplStatement $ astToCFG fixturePythonAssignFunc)

isHooplStatement :: Maybe CFG -> Bool
isHooplStatement Nothing = False
isHooplStatement (Just (CFG n _ _ graph _ _)) = and [assertFunctionName, assertLabelsNotEmpty]
    where assertFunctionName = n == "assign"
          assertLabelsNotEmpty = not $ setNull $ labelsDefined graph

testFunctionCFGHasMoreThanOneLabel:: Test
testFunctionCFGHasMoreThanOneLabel = TestCase $
    assertEqual "one entry label" 1 (setSize $ labelsDefined $ getInternalGraph cfg)
        where Just cfg = astToCFG fixturePythonAssignFunc

tests :: Test
tests = TestList [
      TestLabel "converts source code successfully" testParsesToCFGSuccessfully
    , TestLabel "converts statement to CFG successfully" testCreatesCFGSuccessfully
    , TestLabel "CFG for function has one entry" testFunctionCFGHasMoreThanOneLabel
    ]
