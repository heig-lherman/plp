import System.Environment (getArgs)
import Codegen
import Data.Array.Base (writeArray)

main :: IO ()
main = do
    args <- getArgs
    if null args then
        putStrLn "Usage: compile <source file>"
    else do
        let
            output = head args
            program = []
            instrs = codegen program
        writeFile output (unlines (map show instrs))