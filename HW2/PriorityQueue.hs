module PriorityQueue(Pqueue, 
                     empty, 
                     isEmpty, 
                     insert, 
                     findMin,
                     deleteMin,
                     popMin,
                     fromList,
                     isValid)
where

-- Datatype for priority queues, parameterized around an element type a.
-- The element type should be an instance of the Ord type class.
data Pqueue a =
    Leaf
  | Node a Int (Pqueue a) (Pqueue a)

-- An empty priority queue storing values of type a.
empty :: Pqueue a
empty = Leaf

-- Return True if the queue is empty.
isEmpty :: Pqueue a -> Bool
isEmpty Leaf = True
isEmpty _ = False

-- Returns the integer rank of the priority queue argument.
rank :: Pqueue a -> Int
rank Leaf = 0
rank (Node _ r _ _) = r

-- Merges two priority queues.
-- Thanks for the help in refactoring this!
merge :: Ord a => Pqueue a -> Pqueue a -> Pqueue a
merge x Leaf = x
merge Leaf y = y
merge x@(Node val1 rank1 left1 right1) y@(Node val2 rank2 left2 right2) =
  if val1 < val2
    then if (rank left1) > (rank mergeY)
      then Node val1 (1 + rank mergeY) left1 mergeY
      else Node val1 (1 + rank left1) mergeY left1
    else if (rank left2) > (rank mergeX)
      then Node val2 (1 + rank mergeX) left2 mergeX
      else Node val2 (1 + rank left2) mergeX left2
  where
    mergeY = merge right1 y
    mergeX = merge x right2

-- Insert an item into a priority queue.
insert :: Ord a => a -> Pqueue a -> Pqueue a
insert item q = merge (Node item 1 Leaf Leaf) q

-- Find the minimum-valued element in a priority queue if possible.
findMin :: Ord a => Pqueue a -> Maybe a
findMin Leaf = Nothing
findMin (Node val _ _ _) = Just val

-- Delete the minimum element from a priority queue if possible.
deleteMin :: Ord a => Pqueue a -> Maybe (Pqueue a)
deleteMin Leaf = Nothing
deleteMin (Node _ _ left right) = Just (merge left right)

-- Remove the minimum element if possible and return it, 
-- along with the rest of the priority queue.
popMin :: Ord a => Pqueue a -> Maybe (a, Pqueue a)
popMin Leaf = Nothing
popMin (Node val _ left right) = Just (val, merge left right)

-- Convert an unordered list into a priority queue.
fromList :: Ord a => [a] -> Pqueue a
fromList lst = foldr insert Leaf lst

-- Validate the internal structure of the priority queue.
isValid :: Ord a => Pqueue a -> Bool
isValid Leaf = True
isValid q@(Node val rnk left right) =
    rnk > 0 &&
    rank left >= rank right &&
    rnk == 1 + rank right &&
    lessThanEq val left && lessThanEq val right &&
    isValid left && isValid right
    where
      lessThanEq :: Ord a => a -> Pqueue a -> Bool
      lessThanEq _ Leaf = True
      lessThanEq val1 (Node val2 _ _ _) =  val1 <= val2
