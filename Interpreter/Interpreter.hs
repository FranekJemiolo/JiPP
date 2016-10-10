-- Written by Franciszek Jemioło, index number 346919
-- Modified TestJam for interpreter usage.
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexJam
import ParJam
import SkelJam
import PrintJam
import AbsJam
import Interpret



import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: (Executable a, Print a, Show a) => ParseFun a -> FilePath -> IO ()
runFile p f = readFile f >>= run p

-- Running the program
run :: (Executable a, Print a, Show a) => ParseFun a -> String -> IO ()
run p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrLn s
                          exitFailure
           Ok  tree -> do execProgram tree
                          exitSuccess

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]
  exitFailure

-- Main function - no parameters - running from stdin, 
-- one parameter - running from file
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= run pProgram
    fs -> mapM_ (runFile pProgram) fs





