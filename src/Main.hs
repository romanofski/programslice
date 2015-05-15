import Parse
import Compiler.Hoopl


main :: IO ()
main = do
    c <- getContents
    mapM_ print (runSimpleUniqueMonad $ runWithFuel 0 (parse c))
