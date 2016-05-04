module Lab3ab where

-- PART A

-- Question 1

{-
data Nat = Zero | Succ Nat

instance Eq Nat where
  Zero == Zero = True
  Succ x == Succ y = (x == y)
  _ == _ = False

instance Show Nat where
  show Zero = 0
  show Succ x = (1 + show x)
-}

-- Question 2

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

-- Question 3
instance Ord Nat where
  Zero <= Succ _ = True
  Succ x <= Succ y = (x <= y)
  _ <= _ = False

{-
In this case, having Haskell derive the Ord instance for us will give us the
definition we want, because the derived order is the order of the constructors.
So we end up with Zero < Succ Nat, from which a recursive definition for Ord on
non-Zero Nat's can be found.
-}

-- Question 4

data SignedNat =
  Neg Nat | Pos Nat
  deriving (Show)

instance Eq SignedNat where
  Neg x == Neg y = (x == y)
  Pos x == Pos y = (x == y)
  _ == _ = False

instance Ord SignedNat where
  Neg x <= Neg y = (x >= y)
  Neg _ <= Pos _ = True
  Pos x <= Pos y = (x <= y)
  _ <= _ = False

{-
The automatically-derived definition of Eq would work because equality is
defined for signed numbers in the "standard" way that Haskell can derive.
The constructors are compared, then the contents, and if both are equal, then
the numbers are equal.
However, for Ord, the constructors are given in Neg then Pos order, which works
for comparing a negative number to a positive number, but won't work for
comparing two negative numbers.
-}

-- Question 5

-- Define helper functions first

addNat :: Nat -> Nat -> Nat
addNat Zero x = x
addNat x Zero = x
addNat (Succ x) (Succ y) = Succ (Succ (addNat x y))

mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat _ Zero = Zero
mulNat (Succ Zero) x = x
mulNat x (Succ Zero) = x
mulNat x (Succ y) = addNat x (mulNat x y)

addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Neg Zero) (Neg Zero) = Pos Zero
addSignedNat (Neg Zero) (Pos Zero) = Pos Zero
addSignedNat (Pos Zero) (Neg Zero) = Pos Zero
addSignedNat (Pos Zero) (Pos Zero) = Pos Zero
addSignedNat (Pos Zero) x = x
addSignedNat x (Pos Zero) = x
addSignedNat (Neg Zero) x = x
addSignedNat x (Neg Zero) = x
addSignedNat (Neg x) (Neg y) = Neg (addNat x y)
addSignedNat (Neg (Succ x)) (Pos (Succ y)) = addSignedNat (Neg x) (Pos y)
addSignedNat (Pos (Succ x)) (Neg (Succ y)) = addSignedNat (Pos x) (Neg y)
addSignedNat (Pos x) (Pos y) = Pos (addNat x y)

mulSignedNat :: SignedNat -> SignedNat -> SignedNat
mulSignedNat (Pos x) (Pos y) = Pos (mulNat x y)
mulSignedNat (Pos x) (Neg y) = Neg (mulNat x y)
mulSignedNat (Neg x) (Pos y) = Neg (mulNat x y)
mulSignedNat (Neg x) (Neg y) = Pos (mulNat x y)

negateSignedNat :: SignedNat -> SignedNat
negateSignedNat (Neg x) = Pos x
negateSignedNat (Pos x) = Neg x

absSignedNat :: SignedNat -> SignedNat
absSignedNat (Neg x) = Pos x
absSignedNat x = x

signumSignedNat :: SignedNat -> SignedNat
signumSignedNat (Neg _) = Neg (Succ Zero)
signumSignedNat _ = Pos (Succ Zero)

fromIntegerSignedNat :: Integer -> SignedNat
fromIntegerSignedNat x | x == 0 = Pos Zero
fromIntegerSignedNat x | x < 0 =
  negateSignedNat (fromIntegerSignedNat (abs x))
fromIntegerSignedNat x =
  addSignedNat (Pos (Succ Zero)) (fromIntegerSignedNat (x - 1))

instance Num SignedNat where
  (+) x y = addSignedNat x y
  (-) x y = addSignedNat x (negateSignedNat y)
  (*) x y = mulSignedNat x y
  negate x = negateSignedNat x
  abs x = absSignedNat x
  signum x = signumSignedNat x
  fromInteger x = fromIntegerSignedNat x

-- Question 6

signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos Zero) = 0
signedNatToInteger (Neg Zero) = 0
signedNatToInteger (Pos (Succ x)) = 1 + signedNatToInteger (Pos x)
signedNatToInteger (Neg (Succ x)) = (-1) + signedNatToInteger (Neg x)

-- Question 7

-- Proposed unary encoding for signed integers
-- Uses Succ for successor and Pred for predecessor
data UnaryInteger =
  Zero' | Succ' UnaryInteger | Pred' UnaryInteger

{-
A problem with this representation is the lack of explicit sign information,
since you can have a Pred of several Succ's, which may still be a positive
number, or vice versa with a Succ as a negative number.
-}

-- Question 8
factorial :: (Num a, Ord a) => a -> a
factorial x | x < 0 = error "Negative argument given"
factorial 0 = 1
factorial x = x * (x - 1)

-- PART B

-- Question 1

-- Part 1

{-
Two scores comparison operator: >#<
This is non-associative because the type signature will not work out when
chaining this operator. For example, (1 >#< 2) >#< 3 results in a String being
compared to an Integer, which is not the expect argument types for the operator.
Similarly, 1 >#< (2 >#< 3) results in an Integer compared to a String.
-}

(>#<) :: Integer -> Integer -> String
(>#<) x y | x > y = "First Player"
(>#<) x y | x == y = "Tie"
(>#<) _ _ = "Second Player"
infix >#<

-- Part 2

{-
Last digit of sum operator: +|
This can be either infixr or infixl because the last digit of any sum,
regardless of the number of arguments, is only dependent on the last digits
of those arguments, and addition is an associative operator.
Example: 12 +| 23 +| 34 == (12 +| 23) +| 34 == 12 +| (23 +| 34) == 9
-}

(+|) :: Integer -> Integer -> Integer
(+|) x y = mod 10 (x + y)
infixl +|

-- Part 3

{-
Integer list append operator: &<
This operation is only infixl, because the second argument must be an integer,
not an integer list.
Example: [1] &< 2 &< 3 is [1, 2, 3]
-}

(&<) :: [Integer] -> Integer -> [Integer]
(&<) x y = x ++ [y]
infixl &<

-- Part 4

{-
2x integer cons operator: >&&
This is infixr because cons is infixr only.
1 >&& 2 >&& [3] is [1, 1, 2, 2, 3]
-}

(>&&) :: Integer -> [Integer] -> [Integer]
(>&&) x y = x : x : y
infixr >&&

-- Question 2

{-
This operator will type check in either order of association. Since the two
arguments are the same type (Integer) and the same as the output, we can
associate in either direction. (That is of course, not to say the operator will
function as advertised, however.)

(+#) :: Integer -> Integer -> Integer
(+#)
-}

