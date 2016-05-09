{-
Takes some numbers and a filename as command-line arguments, then outputs the
corresponding columns of the input to standard output. Column N of a line is
defined as the Nth item in the list obtained by splitting on whitespace.
Indexing starts at 1.
-}

module Main where

import System.Environment
import System.Exit
import System.IO
-- import Data.IORef

usage :: IO ()
usage = hPutStrLn stderr $ "usage: columns <positive integers> filename"

-- Parse columns, returns a list of the columns requested
parseCols :: [String] -> [Int]
parseCols [] = []
parseCols [_] = []
parseCols (x:xs) = read x : parseCols xs

-- Validates input arguments except for the last argument
invalidArgs :: [String] -> Bool
invalidArgs [_] = False
invalidArgs [] = True
invalidArgs (x:_) | (read x) <= 0 = True
invalidArgs (_:xs) = invalidArgs xs

-- For every N in a list of integers, prints the Nth word of the wrds argument.
printWords :: [Int] -> [String] -> IO ()
printWords lst wrds = do
  mapM_ putStr [wrds !! (x - 1) ++ " " | x <- lst, x <= length wrds]
  putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  -- if args == [] then usage else do
  if (invalidArgs args)
    -- Arguments not positive, or not enough arguments
    then usage
    -- Numerical arguments are valid
    else do
      -- Parse out the columns
      let cols = parseCols args
      if cols == [] then exitSuccess else do
        let filename = last args
        h <- readFile filename
        let l = lines h
        let wds = map words l
        mapM_ (\x -> printWords cols x) wds

