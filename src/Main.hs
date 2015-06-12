
module Main where

import Data.GraphViz.Printing (renderDot, toDot)
import Language.Python.Version2.Parser (parseModule)
import System.Environment (getArgs)
import Text.Show.Pretty
import qualified Data.Text.Lazy as L (unpack)
import Language.Python.Common.Pretty
import System.Console.GetOpt (usageInfo
                              , ArgDescr(..)
                              , OptDescr(..)
                              , ArgOrder(..)
                              , getOpt)

import Programslice.Parse
import Programslice.Visualisation


data Flag = PrintAST | PrintCFG | DrawDot

options :: [OptDescr Flag]
options =
    [ Option "a" ["ast"] (NoArg PrintAST) "pretty prints AST"
    , Option "i" ["ir"] (NoArg PrintCFG) "prints transformed CFG"
    , Option "d" ["dot"] (NoArg DrawDot) "draws graphviz graph"
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
        (PrintCFG : _) -> do
            c <- getContents
            -- yikes!!! just to print it in escaped form. How to do this
            -- better?
            mapM_ (putStrLn . read . render . ppDoc) (parse c)
        (PrintAST : _) -> do
            c <- getContents
            let Right parsed = parseModule c []
            putStrLn $ ppShow parsed
        (DrawDot : _) -> do
            c <- getContents
            let dot = toDot $ cfgGraphvizRepr $ head (parse c)
            putStrLn $ L.unpack $ renderDot dot
        _ -> ioError (userError $ "No arguments provided. " ++ header)
