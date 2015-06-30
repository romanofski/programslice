module Main where

import Language.Python.Version2.Parser (parseModule)
import System.Environment (getArgs)
import Text.Show.Pretty
import Language.Python.Common.Pretty
import System.Console.GetOpt (usageInfo
                              , ArgDescr(..)
                              , OptDescr(..)
                              , ArgOrder(..)
                              , getOpt)

import Programslice.Parse


data Flag = PrintAST | PrintIR

options :: [OptDescr Flag]
options =
    [ Option "a" ["ast"] (NoArg PrintAST) "pretty prints AST"
    , Option "i" ["ir"] (NoArg PrintIR) "prints transformed IR"
    ]

run :: [String] -> IO [Flag]
run args =
    case getOpt Permute options args of
        (o, _, []) -> return o
        (_, _, err) -> ioError (userError (concat err ++ usageInfo header options))

header :: String
header = "Usage: programslice [i|a]"


main :: IO ()
main = do
    args <- getArgs
    flags <- run args
    case flags of
        (PrintIR : _) -> do
            c <- getContents
            -- yikes!!! just to print it in escaped form. How to do this
            -- better?
            mapM_ (putStrLn . read . render . ppDoc) (parse c)
        (PrintAST : _) -> do
            c <- getContents
            let Right parsed = parseModule c []
            putStrLn $ ppShow parsed
        _ -> ioError (userError $ "No arguments provided. " ++ header)
