module Unit where

import Parse
import Fixtures
import Test.HUnit
import Python.Hoopl (Proc(..))


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

testConvertsSingleFunctionSuccessfully :: Test
testConvertsSingleFunctionSuccessfully = TestCase $
    assertBool "non-empty hoopl statement" (isHooplStatement $ convertSingleStatement fixturePythonAssignFunc)

isHooplStatement :: Proc -> Bool
isHooplStatement (Proc name _ _ _) = name == "assign"

tests :: Test
tests = TestList [
      TestLabel "converts source code successfully" testConvertsToIRSuccessfully
    , TestLabel "converts statement to hoopl correctly" testConvertsToIRSuccessfully
    ]
