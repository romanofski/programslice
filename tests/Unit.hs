{-# LANGUAGE GADTs #-}
module Unit where

import Fixtures
import Test.HUnit
import Compiler.Hoopl

import Programslice.Parse
import Programslice.Visualisation
import Programslice.Python.ControlFlow (CFG(..), Insn)


-- | helper function to read source code from given FilePath and convert
-- it to our IR
--
toIRProcs :: (String -> [a]) -> FilePath -> IO [a]
toIRProcs f p = do
    contents <- readFile p
    return $ f contents

isHooplStatement :: Maybe CFG -> Bool
isHooplStatement Nothing = False
isHooplStatement (Just cfg) = assertLabelsNotEmpty
    where assertLabelsNotEmpty = not $ setNull $ labelsDefined graph
          graph = internalGraph cfg

internalGraph :: CFG -> Graph Insn C C
internalGraph = cfgBody

-- | tests
--
testCreatesCFGSuccessfully :: Test
testCreatesCFGSuccessfully = TestCase $
    assertBool "expecting non-empty CFG graph" (isHooplStatement simpleFunctionFixture)

testGraphIsNotEmpty :: Test
testGraphIsNotEmpty = TestCase $
    assertBool "extracted at least one label" (
      not $ setNull $ labelsDefined $ internalGraph cfg)
    where Just cfg = simpleFunctionFixture

testCFGHasMoreThanOneLabel:: Test
testCFGHasMoreThanOneLabel = TestCase $
    assertEqual "more than one entry label" 2 (setSize $ labelsDefined $ internalGraph cfg)
        where Just cfg = multipleFunctions

tests :: Test
tests = TestList [
      TestLabel "converts statement to CFG successfully" testCreatesCFGSuccessfully
    , TestLabel "CFG for function has one entry" testCFGHasMoreThanOneLabel
    ]
