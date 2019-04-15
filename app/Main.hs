module Main where

import System.Environment
import Algo
import Pars
import System.Random

putUsage :: IO()
putUsage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

putError :: IO()
putError = putStrLn "Bad arg, -h for more information"

parsArgs :: Integer -> Float -> String -> IO()
parsArgs n e filename = do
    fileContent <- readFile filename
    putStrLn (algo n e (parsFile fileContent))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [a, b, c] -> parsArgs (read a :: Integer) (read b :: Float) c
        _ -> putUsage
