import Text.Show.Pretty
import Compiler.Hoopl


main :: IO ()
main = do
    contents <- getContents
    let Right parsed = parseModule contents []
    putStrLn $ ppShow parsed
