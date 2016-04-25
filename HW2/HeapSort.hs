module HeapSort where

import qualified PriorityQueue as Q

-- Sorts a list using heapsort.
sort :: Ord a => [a] -> [a]
sort [] = []
sort lst = reverse $ iter (Q.fromList lst) []
    where
      iter :: Ord a => (Q.Pqueue a) -> [a] -> [a]
      iter q partial | Q.isEmpty q = partial
      iter q partial = iter newQ (val : partial)
          where Just (val, newQ) = Q.popMin q
