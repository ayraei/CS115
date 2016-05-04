module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)

-- Question 1

-- Creates a sparse matrix from a list of index, value pairs and bounds
-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
-- TODO: Check each bound
sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
sparseMatrix lst (rows, cols) | rows < 1 || cols < 1 = error "Bounds invalid!"
sparseMatrix lst (rows, cols) =
  SM (rows, cols)
     (S.fromList rowIdx)
     (S.fromList colIdx)
     (M.fromList lst)
  where
    rowIdx = map (\((x, _), _) -> x) (filter (\(_, val) -> val /= 0) lst)
    colIdx = map (\((_, y), _) -> y) (filter (\(_, val) -> val /= 0) lst)

-- Question 2

-- Adds two compatible sparse matrices
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM a@(SM (abx, aby) ari aci av) b@(SM (bbx, bby) bri bci bv) |
  abx /= bbx || aby /= bby = error "addSM: Incompatible matrices"
-- TODO

-- Question 3

-- Negates a sparse matrix
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM b r c v) = SM b r c (M.map (\x -> negate x) v)

-- Question 4

-- Subtracts two compatible sparse matrices
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM a b = addSM a (negateSM b)

-- Question 5

-- Helper function
{-
mulSMHelp :: (Eq a, Num a) => (M.Map (Integer, Integer) a) ->
  (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a)
-}

-- Multiplies two compatible sparse matrices
mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM a@(SM (abx, aby) ari aci av) b@(SM (bbx, bby) bri bci bv) |
  aby /= bbx = error "mulSM: Incompatible matrices"
-- TODO

-- Question 6

-- Retrieves value from sparse matrix given row and column
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM (abx, aby) _ _ _) (x, y) | x <= 0 || y <= 0 || x > abx || y > aby =
  error "getSM: Request index out of bounds"
getSM (SM _ _ _ av) k | (M.lookup k av) == Nothing = 0
getSM (SM _ _ _ av) k = x where
  (Just x) = M.lookup k av

-- Returns the number of rows in a sparse matrix
rowsSM :: SparseMatrix a -> Integer
rowsSM (SM (rows, _) _ _ _) = rows

-- Returns the number of columns in a sparse matrix
colsSM :: SparseMatrix a -> Integer
colsSM (SM (_, cols) _ _ _) = cols

-- Question 7

-- Operator shortcuts for addSM, subSM, mulSM, getSM, respectively

(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) x y = addSM x y

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) x y = subSM x y

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) x y = mulSM x y

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) sm idx = getSM sm idx

-- Question 8

{-
The Num type class requires a fromInteger method, which makes no sense
for a sparse matrix.
-}
