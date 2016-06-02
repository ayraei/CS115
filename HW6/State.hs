import Control.Monad
import Control.Monad.State
import Data.IORef


-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)

-- PART A

-- Question 1

-- Uses the IO monad to compute the factorial of the input.
factIO :: Integer -> IO Integer
factIO n | n < 0 = error "Invalid input!"
factIO n =
  do
    counter <- newIORef n
    total <- newIORef 1
    whileIO
      (do counter' <- readIORef counter
          return (counter' /= 0))
      (do counter' <- readIORef counter
          total' <- readIORef total
          writeIORef counter (counter' - 1)
          writeIORef total (total' * counter'))
    readIORef total

-- Question 2

-- Uses a state monad to compute the factorial of the input.
factState :: Integer -> Integer
factState n | n < 0 = error "Invalid input!"
factState n = evalState factStateHelper (n, 1) where
  factStateHelper :: State (Integer, Integer) Integer
  factStateHelper = do
    whileState (\(counter, _) -> counter /= 0)
      (do (counter, total) <- get
          put (counter - 1, total * counter))
    (_, total) <- get
    return total

-- Question 3

-- Uses the IO monad to compute fibonacci numbers.
fibIO :: Integer -> IO Integer
fibIO n | n < 0 = error "Invalid input!"
fibIO n =
  do
    counter <- newIORef n
    f0 <- newIORef 0
    f1 <- newIORef 1
    whileIO
      (do counter' <- readIORef counter
          return (counter' /= 0))
      (do counter' <- readIORef counter
          f0' <- readIORef f0
          f1' <- readIORef f1
          writeIORef counter (counter' - 1)
          writeIORef f0 f1'
          writeIORef f1 (f0' + f1'))
    readIORef f0

-- Question 4

-- Uses a state monad to compute fibonacci numbers.
fibState :: Integer -> Integer
fibState n | n < 0 = error "Invalid input!"
fibState n = evalState fibStateHelper (n, 0, 1) where
  fibStateHelper :: State (Integer, Integer, Integer) Integer
  fibStateHelper = do
    whileState (\(counter, _, _) -> counter /= 0)
      (do (counter, f0, f1) <- get
          put (counter - 1, f1, f0 + f1))
    (_, f0, _) <- get
    return f0


-- PART B

{-
I... honestly had a really hard time understanding/writing up the proof for
this problem.

Note: >>= operator must support function composition (>=>)

Assume two functions in Reader monad with signatures:
f :: a -> Reader r b
g :: b -> Reader r c

We would like to compose them to give a function with signature:
h :: a -> Reader r c

Non-monadic forms of f, g, h:
f' :: (a, r) -> b
g' :: (b, r) -> c
h' :: (a, r) -> c
h' x =
  let y = f' (r, x)
      z = g' (r, y)
  in (r, z)

We have
h = f >=> g
equivalent to
h x = f x >>= g
equivalent to
h x = f x >>= \y -> g y
equivalent to
h x = do y <- f x
         g y

Let
f'' :: a -> r -> b
f'' x = f' (x, rd)
g'' :: b -> r -> c
g'' y = g' (y, rd)

f :: a -> Reader r b
f x = Reader 

...something happens in between...

mx >>= f = Reader (\r ->
             let x = runReader mx r in
               runReader (f x) r)

return is conceptually the identity function

return :: a -> Reader r a
return x = Reader (\r -> x)
-}

