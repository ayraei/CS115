--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (0, 0)
  where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter s (x, y) | readArray s (x, y) /= 0 =
      iter s ((x + 1) `mod` 9, if x == 8 then y + 1 else y)
    iter s (x, y) = iter' s (x, y) (getOKValues s (x, y))

    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' s (x, y) [] = do
      writeArray s (x, y) 0
      return False
    iter' s (x, y) (n:ns) = do
      writeArray s (x, y) n
      let newX = if x /= 8 then x + 1 else 0
      let newY = if newX == 0 then y + 1 else y
      if iter s (newX, newY)
        then return True
        else iter' s (x, y) ns
{-
    iter' s (x, y) validMoves =
      map f (getOKValues s (x, y))
-}

    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues s (x, y) = do
      r <- getRow s x
      c <- getCol s y
      b <- getBox s (x, y)
      let l = [1..9]
      return (filter
        (\e -> not ((e `elem` r) || (e `elem` c) || (e `elem` b))) l)

    -- Return the ith row in a Sudoku board as a list of Ints.
    getRow :: Sudoku -> Int -> IO [Int]
    getRow s row = do
      let l = [0..8]
      mapM_ (\col -> readArray s (row, col)) l
      return l

    -- Return the ith column in a Sudoku board as a list of Ints.
    getCol :: Sudoku -> Int -> IO [Int]
    getCol s col = do
      let l = [0..8]
      mapM_ (\row -> readArray s (row, col)) l
      return l

    -- Return the box covering location (i, j) as a list of Ints.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox s (x, y) = do
      let r = if x `elem` [0, 1, 2] then [0, 1, 2]
             else if x `elem` [3, 4, 5] then [3, 4, 5]
                  else [6, 7, 8]
      let c = if y `elem` [0, 1, 2] then [0, 1, 2]
             else if y `elem` [3, 4, 5] then [3, 4, 5]
                  else [6, 7, 8]
      let pos = [(row, col) | row <- r, col <- c]
      mapM_ (\p -> readArray s p) pos
      return pos

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure

