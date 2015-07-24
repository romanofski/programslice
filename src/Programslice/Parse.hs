-- | Parsing Language X to Intermediate Representation
-- TODO: all functions here will need to change in order to support a
-- more flexible conversion.
--
module Programslice.Parse where

import Language.Python.Version2.Parser
import Language.Python.Common.SrcLocation
import Language.Python.Common (Token)
import Language.Python.Common.AST

import Programslice.Python.ControlFlow (CFG, moduleToCFG)


-- | Monadic code to convert source code to a list of IR procedures.
--
parse :: String -> Maybe CFG
parse contents =
    case parseModule contents [] of
        Right parsed -> moduleToCFG $ getAST parsed
        Left _ -> Nothing
    where
        getAST :: (Module SrcSpan, [Token]) -> Module SrcSpan
        getAST (x, _) = x
