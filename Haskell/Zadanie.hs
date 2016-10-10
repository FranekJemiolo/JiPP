-- This program was written by Franciszek Jemioło identified by index number
-- 346919.
module Main where
import Prelude
import MyArray
import Graph
import System.IO
import System.Environment(getArgs)

-- Reads input and returns list of strings
main = do
    args <- getArgs
    case args of
        [] -> do
            contents <- getContents
            putStrLn $ listConnected $ readGraph contents
        [fileName] -> do
            handle <- openFile fileName ReadMode
            contents <- hGetContents handle
            putStrLn $ listConnected $ readGraph contents
            hClose handle
        _ -> error "Wrong arguments passed!" 