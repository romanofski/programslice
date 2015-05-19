-- | Parsing Language X to Intermediate Representation
-- TODO: all functions here will need to change in order to support a
-- more flexible conversion.
--
module Parse where

import Language.Python.Version2.Parser
import Language.Python.Common.SrcLocation
import Language.Python.Common (Token)
import Language.Python.Common.AST
import PythonAst2IR (astToIR)
import Compiler.Hoopl
import qualified IR as I


getAST :: (Module SrcSpan, [Token]) -> [Statement SrcSpan]
getAST (Module xs, _) = xs

-- | Helper function to check if we can convert ASTs correctly to our
-- internal representation.
--
-- /TODO:/ don't stick to just functions once we extend our IR
--
parsedToFun :: [Statement SrcSpan] -> [Statement SrcSpan]
parsedToFun (fun@(Fun{}):xs) = fun : parsedToFun xs
parsedToFun [] = []
parsedToFun (_:xs) = parsedToFun xs


parse :: String -> SimpleFuelMonad [I.Proc]
parse contents =
    case parseModule contents [] of
        Right parsed -> mapM astToIR $ parsedToFun $ getAST parsed
        Left _ -> return []

-- | converts the source code given as a string to our intermediate
-- representation
--
convert :: String -> [I.Proc]
convert contents = runSimpleUniqueMonad $ runWithFuel 0 (parse contents)
