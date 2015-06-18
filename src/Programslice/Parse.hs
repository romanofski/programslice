-- | Parsing Language X to Intermediate Representation
-- TODO: all functions here will need to change in order to support a
-- more flexible conversion.
--
module Programslice.Parse where

import Language.Python.Version2.Parser
import Language.Python.Common.SrcLocation
import Language.Python.Common (Token)
import Language.Python.Common.AST
import Data.Maybe (mapMaybe)

import Programslice.Python.ControlFlow (CFG, astToCFG)


-- | Helper function to filter out all functions from a list of
-- statements.
--
filterFunction :: [Statement SrcSpan] -> [Statement SrcSpan]
filterFunction (fun@(Fun{}):xs) = fun : filterFunction xs
filterFunction [] = []
filterFunction (_:xs) = filterFunction xs


-- | Monadic code to convert source code to a list of IR procedures.
--
parse :: String -> [CFG]
parse contents =
    case parseModule contents [] of
        Right parsed -> mapMaybe astToCFG $ filterFunction $ getAST parsed
        Left _ -> []
    where
        getAST :: (Module SrcSpan, [Token]) -> [Statement SrcSpan]
        getAST (Module xs, _) = xs