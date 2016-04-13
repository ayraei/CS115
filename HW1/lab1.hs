-- TODO: A3

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
                 | otherwise = iter x y 1
                     where
                       iter :: Integer -> Integer -> Integer -> Integer
                       iter x y prod | x == y    = prod * y
                                     | otherwise = iter x (y - 1) (prod * y)

-- Question 3

-- Returns the product of all numbers in a list, or 1 if empty list.
prod :: [Integer] -> Integer
prod (x:xs) = 1

-- Computes the product of all integers between x and y (inclusive)
rangeProduct' :: Integer -> Integer -> Integer
rangeProduct' x y | x > y = error "First argument cannot be less than second."
                  | otherwise = prod [1, 2]

-- Question 4

-- Part 1

-- Part 2

-- Part 3

{-
dot :: [Integer] -> [Integer] -> Integer
dot lst1 lst2 = sum (map2 (*) lst1 lst2)

dot :: [Integer] -> [Integer] -> Integer
dot = (sum .) . map2 (*)
-}

-- Question 5

-- Question 6

-- PART B

-- Question 1

sumList :: [Integer] -> Integer
sumList [] = 0
sumList lst = head lst + sumList (tail lst)

{-

-}

-- Question 2

-- Return the largest value in a list of integers.
largest :: [Integer] -> Integer
largest xs | length xs == 0 = error "empty list"
largest xs | length xs == 1 = head xs
largest xs = max (head xs) (largest (tail xs))

{-

-}

-- PART C

-- Question 1

-- Question 2

-- Question 3

-- Question 4

-- Question 5

-- Question 6
