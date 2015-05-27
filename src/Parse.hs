-- | Parsing Language X to Intermediate Representation
-- TODO: all functions here will need to change in order to support a
-- more flexible conversion.
--
module Parse where

import Language.Python.Version2.Parser
import Language.Python.Common.SrcLocation
import Language.Python.Common (Token)
import Language.Python.Common.AST
import Python.ControlFlow (astToIR)
import Python.Hoopl (Proc)
import Compiler.Hoopl


-- | Helper function to filter out all functions from a list of
-- statements.
--
filterFunction :: [Statement SrcSpan] -> [Statement SrcSpan]
filterFunction (fun@(Fun{}):xs) = fun : filterFunction xs
filterFunction [] = []
filterFunction (_:xs) = filterFunction xs


-- | Monadic code to convert source code to a list of IR procedures.
--
parse :: String -> SimpleFuelMonad [Proc]
parse contents =
    case parseModule contents [] of
        Right parsed -> mapM astToIR $ filterFunction $ getAST parsed
        Left _ -> return []
    where
        getAST :: (Module SrcSpan, [Token]) -> [Statement SrcSpan]
        getAST (Module xs, _) = xs

-- | Converts single statement to hoopl
--
convertSingleStatement :: Statement SrcSpan -> Proc
convertSingleStatement stm = runSimpleUniqueMonad $ runWithFuel 0 (astToIR stm)

-- | converts the source code given as a string to our intermediate
-- representation
--
convert :: String -> [Proc]
convert contents = runSimpleUniqueMonad $ runWithFuel 0 (parse contents)
