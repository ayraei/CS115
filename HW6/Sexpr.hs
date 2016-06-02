--
-- S-expression parser.
--

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA   Bool
  | IntA    Integer
  | FloatA  Double
  | IdA     String  -- identifier
  | StringA String
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  ex <- option "" (string "E")
  ex <- option ex (string "e")
  if ex /= "" then do
    sign2 <- option "" (string "-")
    sign2 <- option sign2 (string "+")
    digits2 <- many1 digit
    let ex = ex ++ sign2 ++ digits2
    return (read (sign ++ digits ++ "." ++ f ++ ex) :: Double)
  else return (read (sign ++ digits ++ "." ++ f) :: Double)
  <?> "floating-point number"

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

parseString :: Parser String
parseString = do
  char '\"'
  s <- many1 (noneOf "\"")
  char '\"'
  return s
  <?> "string"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> (parseString >>= return . StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- Parse a list of S-expressions, delimited by parentheses,
-- separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList = (do
  char '('
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  char ')'
  return ss)
  <|> (do
    char '['
    optional parseSep
    ss <- parseSexpr `sepEndBy` parseSep
    char ']'
    return ss)
  <|> (do
    char '{'
    optional parseSep
    ss <- parseSexpr `sepEndBy` parseSep
    char '}'
    return ss)
  <?> "list of S-expressions"
  
{-
Question 3

Note that you don't need to use the try combinator in the parseList function
in the event that you tried to parse one kind of delimiter and failed.
Why not?

The parseSep parser will consume as many spaces as it can and return; if there
are no spaces, it will not consume anything, so does not need to backtrack.
The rest of parseList just gets sent through parseSexpr, which has try
statements as necessary.
-}

-- Parse a quoted expression.
parseQuote :: Parser [Sexpr]
parseQuote = do
  char '\''
  rest <- parseSexpr
  return ([AtomS (IdA "quote")] ++ [rest])
  <?> "quoted S-expression"

-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseQuote >>= return . ListS)
  <|> (parseList >>= return . ListS)
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
--ppSexpr i (AtomS (IdA "quote")) = indent i ++ "\'"
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"
{-
removed this section for pretty-printing Quotes because they are also IdA's
ppSexpr i (QuoteS s)  = indent i ++ 
  "QuoteS[\n" 
  ++ ppSexpr (i + 2) s 
  ++ "\n"
  ++ indent i
  ++ "]"
-}

-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"

