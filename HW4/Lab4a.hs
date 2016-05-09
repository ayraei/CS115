-- Need to import this to get the definition of toUpper:
import Data.Char

-- PART A

-- Question 1

-- Part 1

{-
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = do
  putChar c
  myPutStrLn cs
-}

myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) =
  putChar c >> myPutStrLn cs

-- Part 2

{-
greet :: String -> IO ()
greet name = do putStrLn ("Hello, " ++ name ++ "!")
-}

greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- Part 3

{-
-- Ask the user for his/her name, then print a greeting.
greet2 :: IO ()
greet2 = do
  putStr "Enter your name: "
  name <- getLine
  putStr "Hello, "
  putStr name
  putStrLn "!"
-}

-- Simple desugar
greet2 :: IO ()
greet2 =
  putStr "Enter your name: "
  >> getLine >>= \name -> putStr "Hello, " >> putStr name >> putStrLn "!"

-- Complex desugar
greet2' :: IO ()
greet2' =
  putStr "Enter your name: "
  >> getLine >>=
  \y -> case y of
    name@(_:_) -> putStr "Hello, " >> putStr name >> putStrLn "!"
    _ -> fail "Pattern match failure in do expression"

{-
The complex desugaring does behave a little differently from the simple
desugaring. If no characters are entered before hitting return/newline in the
complex desugaring, the function will print an error. In the simple desugar,
a newline entry will cause the function to print "Hello, !"
-}

-- Part 4

{-
-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
greet3 :: IO ()
greet3 = do
  putStr "Enter your name: "
  (n:ns) <- getLine
  let name = toUpper n : ns
  putStr "Hello, "
  putStr name
  putStrLn "!"
-}

-- Simple desugar
greet3 :: IO ()
greet3 =
  putStr "Enter your name: "
  >> getLine >>= \(n:ns) ->
    let name = toUpper n : ns in
      putStr "Hello, " >> putStr name >> putStrLn "!"

-- Complex desugar
greet3' :: IO ()
greet3' =
  putStr "Enter your name: "
  >> getLine >>=
  \y -> case y of
    (n:ns) -> let name = toUpper n : ns in
      putStr "Hello, " >> putStr name >> putStrLn "!"
    _ -> fail "Pattern match failure in do expression"

{-
The complex desugaring again makes sure that an empty input will cause an
appropriate error message to print out.
-}

