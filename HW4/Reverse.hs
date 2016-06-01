-- Reads in all lines of a file and prints them in reverse order.

module Main where

import System.Environment
import System.Exit

main :: IO ()
main =
  getArgs >>=
  \y -> case y of
    -- Filename supplied as argument
    [filename] -> do
      h <- readFile filename
      let l = lines h
      mapM_ putStrLn (reverse l)
      exitSuccess
    -- No arguments provided
    _ -> do
      putStr "usage: "
      prog <- getProgName
      putStr prog
      putStrLn " filename"
      exitFailure

