module Unit where

import Fixtures
import Test.HUnit

import Programslice.Parse
import Programslice.Python.ControlFlow (astToCFG, CFG(..))


-- | helper function to read source code from given FilePath and convert
-- it to our IR
--
toIRProcs :: (String -> [a]) -> FilePath -> IO [a]
toIRProcs f p = do
    contents <- readFile p
    return $ f contents

testConvertsToIRSuccessfully :: Test
testConvertsToIRSuccessfully = TestCase $ do
    procs <- toIRProcs parse "tests/data/binsearch.py"
    assertBool "non-empty list expected" (not $ null procs)

testConvertsSingleFunctionSuccessfully :: Test
testConvertsSingleFunctionSuccessfully = TestCase $
    assertBool "non-empty hoopl statement" (isHooplStatement $ astToCFG fixturePythonAssignFunc)

isHooplStatement :: Maybe CFG -> Bool
isHooplStatement Nothing = False
isHooplStatement (Just (CFG name _ _ _ _)) = name == "assign"

tests :: Test
tests = TestList [
      TestLabel "converts source code successfully" testConvertsToIRSuccessfully
    , TestLabel "converts statement to hoopl correctly" testConvertsToIRSuccessfully
    ]
