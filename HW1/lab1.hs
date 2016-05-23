-- PART A

-- Question 1

-- Computes the sum of the squares of its arguments
(+*) :: Double -> Double -> Double
(+*) x y = (x * x) + (y * y)
infixl 7 +*

-- Computes exclusive-OR of its two boolean arguments
(^||) :: Bool -> Bool -> Bool
(^||) x y | x == False = y
          | otherwise  = not y
infixr 3 ^||

-- Question 2

-- Computes the product of all integers between x and y (inclusive)
rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | x > y = error "First argument cannot be less than second."
                 | x == y = y
                 | otherwise = x * rangeProduct (x + 1) y

-- Question 3

-- Returns the product of all numbers in a list, or 1 if empty list.
prod :: [Integer] -> Integer
prod = foldr (*) 1

-- Computes the product of all integers between x and y (inclusive)
rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 x y | x > y = error "First argument cannot be less than second."
                  | otherwise = prod [x..y]

-- Question 4

-- Part 1

-- Maps a two-argument function over two lists
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = (f x y) : map2 f xs ys

-- Part 2

-- Maps a three-argument function over three lists
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] _ _ = []
map3 _ _ [] _ = []
map3 _ _ _ [] = []
map3 f (x:xs) (y:ys) (z:zs) = (f x y z) : map3 f xs ys zs

-- Part 3

{-
dot :: [Integer] -> [Integer] -> Integer
dot lst1 lst2 = sum (map2 (*) lst1 lst2)

dot :: [Integer] -> [Integer] -> Integer
dot = (sum .) . map2 (*)

Evaluation of point-free dot lst1 lst2
--> (sum .) . map2 (*) lst1 lst2
--> (sum .) $ map2 (*) lst1 lst2
--> (\x -> sum . x) $ map2 (*) lst1 lst2
--> sum $ map2 (*) lst1 lst2
--> sum (map2 (*) lst1 lst2)
-}

-- Question 5

-- Computes the sum of the numbers [0, 999] which are multiples of 3 or 5
ans1 = sum [x | x <- [0..999], mod x 3 == 0 || mod x 5 == 0]
-- ans1 = 233168

-- Question 6

-- Computes the sum of all prime numbers below 10000

-- Uses the Sieve of Eratosthenes algorithm to generate an infinite primes list
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]

primes = sieve [2..]

-- Takes primes under 10000 and computes their sum
ans2 = sum $ takeWhile (< 10000) primes
-- ans2 = 5736396

-- PART B

-- Question 1

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

{-
It would be more natural to use pattern matching instead of the head and tail
functions. So, we removed the use of head and tail and wrote the argument lst
as (x:xs).
-}

-- Question 2

-- Return the largest value in a list of integers.
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [x] = x
largest (x:xs) = max x (largest xs)

{-
It would be more natural and efficient to use pattern matching and match
directly on the argument to 'largest', instead of the head and tail
functions. So, we changed the arguments for the top two cases to use
pattern matching instead of checking length, and we changed the general
case to use (x:xs).
-}

-- PART C

-- Question 1

{-
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

Evaluate fib 3
--> fib (3 - 1) + fib (3 - 2)
--> fib 2 + fib (3 - 2)
--> (fib (2 - 1) + fib (2 - 2)) + fib (3 - 2)
--> (fib 1 + fib (2 - 2)) + fib (3 - 2)
--> (1 + fib (2 - 2)) + fib (3 - 2)
--> (1 + fib 0) + fib (3 - 2)
--> (1 + 0) + fib (3 - 2)
--> 1 + fib (3 - 2)
--> 1 + fib 1
--> 1 + 1
--> 2
-}

-- Question 2

{-
fact :: Integer -> Integer
fact n = n * fact (n - 1)
fact 0 = 1

Evaluate fact 3
--> 3 * fact (3 - 1)
--> 3 * fact 2
--> 3 * (2 * fact (2 - 1))
--> 3 * (2 * fact 1)
--> 3 * (2 * (1 * fact (1 - 1)))
--> 3 * (2 * (1 * fact 0))
--> 3 * (2 * (1 * (0 * fact (0 - 1))))
--> 3 * (2 * (1 * (0 * fact (-1))))
--> ...and so on.

This evaluation will never terminate due to the order in which the cases are
written. Due to lazy evaluation, the first (recursive) case will always be
reached and matched before the fact 0 = 1 case, which must be matched at some
point for the evaluation to terminate. To fix this definition, switch the
order so that fact 0 = 1 comes before the "fact n" case.
(Also, the definition even when fixed will not be correct for negative numbers,
when it should be an error as it is not defined for negatives. However, that
did not seem to be the focus of this question.)
-}

-- Question 3

{-
reverse :: [a] -> [a]
reverse xs = iter xs []
  where
    iter :: [a] -> [a] -> [a]
    iter [] ys = ys
    iter (x:xs) ys = iter xs (x:ys)

Evaluate reverse [1,2,3]
--> iter [1,2,3] []
--> iter [2,3] (1:[])
--> iter [3] 2:(1:[])
--> iter [] 3:(2:(1:[]))
--> 3:(2:(1:[]))
--> 3:(2:[1])
--> 3:[2,1]
--> [3,2,1]

The asymptotic time complexity of the function is O(n), where n is
the length of the list. Essentially there are two passes through the full
list. In the first pass, each subsequent element is removed from the original
list and cons'd to the beginning of the reversed list. In the second pass,
all the cons calls are evaluated to generate the final list. Additionally,
the two passes cannot be performed at the same time, so we have 2n --> O(n).
-}

-- Question 4

{-
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

Evaluate reverse [1, 2, 3]
--> reverse [2, 3] ++ [1]
--> (reverse [3] ++ [2]) ++ [1]
--> ((reverse [] ++ [3]) ++ [2]) ++ [1]
--> (([] ++ [3]) ++ [2]) ++ [1]
--> ([3] ++ [2]) ++ [1]
--> [3, 2] ++ [1]
--> [3, 2, 1]

For a list of length n, there are n substitutions of the recursive case of
reverse, and n append operations of up to n operations each. This gives us
n + n^2 operations ==> O(n^2).
-}

-- Question 5

{-
isort :: [Integer] -> [Integer]
isort [] = []
isort (n:ns) = insert n (isort ns)
  where
    insert :: Integer -> [Integer] -> [Integer]
    insert n [] = [n]
    insert n m@(m1:_) | n < m1 = n : m
    insert n (m1:ms) = m1 : insert n ms

head :: [a] -> a
head [] = error "empty list"
head (x:_) = x

Evaluate head (isort [3, 1, 2, 5, 4])
--> head (insert 3 (isort [1, 2, 5, 4]))
--> head (insert 3 (insert 1 (isort [2, 5, 4])))
--> head (insert 3 (insert 1 (insert 2 (isort [5, 4]))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
--> head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
--> head (insert 3 (insert 1 (2 : 4 : insert 5 [])))
--> head (insert 3 (1 : 2 : 4 : insert 5 []))
--> head (1 : insert 3 (2 : 4 : insert 5 []))
--> 1
-}

-- Question 6

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init 
foldr f init (x:xs) = f x (foldr f init xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ init [] = init
foldl f init (x:xs) = foldl f (f init x) xs

Evaluate foldr max 0 [1, 5, 3, -2, 4]
--> max 1 (foldr max 0 [5, 3, -2, 4])
--> max 1 (max 5 (foldr max 0 [3, -2, 4]))
--> max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
--> max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
--> max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
--> max 1 (max 5 (max 3 (max -2 (max 4 0))))
--> max 1 (max 5 (max 3 (max -2 4)))
--> max 1 (max 5 (max 3 4))
--> max 1 (max 5 4)
--> max 1 5
--> 5

Evaluate foldl max 0 [1, 5, 3, -2, 4]
--> foldl max (max 0 1) [5, 3, -2, 4]
--> foldl max (max (max 0 1) 5) [3, -2, 4]
--> foldl max (max (max (max 0 1) 5) 3) [-2, 4]
--> foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
--> foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
--> max (max (max (max (max 0 1) 5) 3) -2) 4
--> max (max (max (max 1 5) 3) -2) 4
--> max (max (max 5 3) -2) 4
--> max (max 5 -2) 4
--> max 5 4
--> 5


The space complexity of foldr can be smaller than that of foldl if the
argument f given to foldr can be lazily evaluated. The function max cannot be
lazily evaluated, so here there is no difference between the two evaluations.
-}
