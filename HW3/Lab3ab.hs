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
  Zero <= Succ x = True
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
  //

{-
Automatically-derived definitions of Eq and Ord
-}

-- Question 5

-- Question 6

-- Question 7

-- Question 8

-- PART B


