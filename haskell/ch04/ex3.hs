import System.Environment (getArgs)
import Data.List

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"
          myFunction = transform
                        
(|>) x f = f x    

safeHead [] = []
safeHead (x:xs) = x                                                 

transform input = input |> lines |> map words |> map safeHead |> unlines