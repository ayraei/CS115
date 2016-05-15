import Control.Monad

-- PART A

-- Question 1


-- Finds positive integers that can be expressed as the sum of two cubes in
-- two different ways.
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions =
  do
    i <- [1..]
    j <- [1..i-1]
    k <- [1..j-1]
    l <- [1..k-1]
    guard (i^3 + l^3 == j^3 + k^3)
    return ((i, l), (j, k), i^3 + l^3)

-- Question 2

-- Computes the sum of the natural numbers below one thousand which are
-- multiples of 3 or 5. Uses guard.
underThousand :: Integer
underThousand =
  sum $ do
    i <- [1..999]
    guard $ (mod i 3) == 0 || (mod i 5) == 0
    return i

-- Computes the sum of the natural numbers below one thousand which are
-- multiples of 3 or 5. Does not use the guard function explicitly.
underThousand' :: Integer
underThousand' =
  sum $ do
    i <- [1..999]
    if (mod i 3) == 0 || (mod i 5) == 0 then return () else mzero
    return i

-- Result from both methods: 233168

-- Question 3

-- Finds the largest palindrome made from the product of two 3-digit numbers.
-- Returns a 3-tuple of the two 3-digit numbers, then the palindrome.
palindromeFromSum :: (Integer, Integer, Integer)

-- Returns True if the integer is a palindrome.
isPalindrome :: Integer -> Bool

-- The solution: TODO

-- Question 4

-- Given the digits 1 to 9 in sequence, put a '+', '-' or nothing between each
-- digit to get an arithmetic expression.
-- Finds all expressions of this type that evaluate to 100.

type Expr = [Item]

data Item = N Int | O Op
  deriving Show

data Op = Add | Sub | Cat
  deriving Show

ops :: [Item]
ops = [O Add, O Sub, O Cat]

-- Part 1

-- List of all possible valid expressions from the puzzle description
exprs :: [Expr]
exprs =

-- Part 2

-- Takes an expression and removes all instances of Cat operator
normalize :: Expr -> Expr
normalize =

-- Part 3

-- Takes a normalize expression and evaluates it to an Int
evaluate :: Expr -> Int
evaluate =

-- Given as part of the assignment

-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs

-- PART B

-- Question 1

{-
Why does this expression:

do n1 <- [1..6]
   n2 <- [1..6]
   []
   return (n1, n2)

evaluate to []? Show all the steps in your derivation.
-}

-- Question 2

{-
Why does this expression:

do n1 <- [1..6]
   n2 <- [1..6]
   return <anything>
   return (n1, n2)

return the same thing as this expression:

do n1 <- [1..6]
   n2 <- [1..6]
   return (n1, n2)

? Answer this by reducing both expressions to the same expression.
Show all the steps in your derivations.
-}

-- Question 3

{-
You can use the list monad to perform simple pattern-matching tasks. Consider this code:

let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s 
     return [c1, c2]

This returns this result:

["xy", "zw", "cc"]

Explain why by deriving the result, using the full case-style desugaring of do
expressions we covered in lecture 11. Note that the fail method of the
Monad type class has this definition in the list monad:

fail _ = []

Explain what would happen if instead fail for the list monad used the
default definition given in the Monad type class, which is:

fail s = error s

Don't forget that the String datatype in Haskell is just a list of
Chars i.e. [Char].
-}

-- Question 4

{-
Lecture 15 we mentioned that in GHC the real definition of >>= for lists is:

  m >>= k = foldr ((++) . k) [] m

and we claimed that this is the same as the definition we used:

  m >>= k = concat (map k m)

Show that the two expressions foldr ((++) . k) [] m and concat (map k m) do
indeed compute the same thing.

Hint: Show that given m = [x1, x2, ...] both expressions evaluate to the same
thing. Also show this for m = []. Write your answer in a comment, as usual.

Hint: Expand   (++) . k   into an explicit lambda expression.
-}

-- Question 5

{-
Ben Bitfiddle's generic addition attempt.

{-# LANGUAGE ExistentialQuantification #-}

module Sum where

data AnyNum = forall a . Num a => AnyNum a

anySum :: [AnyNum] -> AnyNum
anySum [] = AnyNum 0
anySum ((AnyNum n) : ns) =
  case anySum ns of
    AnyNum s -> AnyNum (n + s)

This doesn't work, returning an error message of:

Sum.hs:11:29:
    Could not deduce (a1 ~ a)
    from the context (Num a)
      bound by a pattern with constructor
                 AnyNum :: forall a. Num a => a -> AnyNum,
               in an equation for `anySum'
      at Sum.hs:9:10-17
    or from (Num a1)
      bound by a pattern with constructor
                 AnyNum :: forall a. Num a => a -> AnyNum,
               in a case alternative
      at Sum.hs:11:5-12
      `a1' is a rigid type variable bound by
           a pattern with constructor
             AnyNum :: forall a. Num a => a -> AnyNum,
           in a case alternative
           at Sum.hs:11:5
      `a' is a rigid type variable bound by
          a pattern with constructor
            AnyNum :: forall a. Num a => a -> AnyNum,
          in an equation for `anySum'
          at Sum.hs:9:10
    In the second argument of `(+)', namely `s'
    In the first argument of `AnyNum', namely `(n + s)'
    In the expression: AnyNum (n + s)

TODO: Explanation
-}
