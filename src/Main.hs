import Language.Python.Version2.Parser
import Language.Python.Common.SrcLocation
import Language.Python.Common (Token)
import Language.Python.Common.AST
import PythonAst2IR (astToIR)
import Compiler.Hoopl
import qualified IR as I


getAST :: (Module SrcSpan, [Token]) -> [Statement SrcSpan]
getAST (Module xs, _) = xs

parsedToFun :: [Statement SrcSpan] -> [Statement SrcSpan]
parsedToFun (fun@(Fun{}):xs) = fun : parsedToFun xs
parsedToFun [] = []
parsedToFun (_:xs) = parsedToFun xs


convertToIR :: [Statement SrcSpan] -> SimpleFuelMonad [I.Proc]
convertToIR = mapM astToIR


parse :: String -> SimpleFuelMonad [I.Proc]
parse contents =
    case parseModule contents [] of
        Right parsed -> convertToIR $ parsedToFun $ getAST parsed
        Left _ -> return []

main :: IO ()
main = do
    c <- getContents
    mapM_ print (runSimpleUniqueMonad $ runWithFuel 0 (parse c))
