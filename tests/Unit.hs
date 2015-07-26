{-# LANGUAGE GADTs #-}
module Unit where

import Fixtures
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Compiler.Hoopl

import Programslice.Visualisation (buildGraphNodeLabels)
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
unitTests :: TestTree
unitTests = testGroup "CFG tests"
            [ testGraphIsNotEmpty
            , testCFGHasMoreThanOneLabel
            ]

testGraphIsNotEmpty :: TestTree
testGraphIsNotEmpty = testCase "extracted at least one label" $ assertBool "graph is not empty" (
      not $ setNull $ labelsDefined $ internalGraph cfg)
    where Just cfg = simpleFunctionFixture

testCFGHasMoreThanOneLabel:: TestTree
testCFGHasMoreThanOneLabel = testCase "more than one entry label" $
    assertEqual "expecting more than one label" 2 (setSize $ labelsDefined $ internalGraph cfg)
        where Just cfg = multipleFunctions

-- | tests Visualisation.hs
--
testBuildsGraphLabels :: TestTree
testBuildsGraphLabels = testCase "more than one label" $
                        assertEqual "expecting 2 labels" 2 (length xs)
  where Just cfg = multipleFunctions
        xs = buildGraphNodeLabels cfg

tests :: TestTree
tests = testGroup "unit tests" [unitTests]
