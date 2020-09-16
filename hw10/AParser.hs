 module AParser () where

import           Control.Applicative

import           Data.Char


{-
  > baz ord (char 'A') "ABC"
  Just (65,"BC")

  > :t baz
  baz :: (a -> b) -> Parser a -> String -> Maybe (b, String)

  :t runParser
  runParser :: Parser a -> String -> Maybe (a, String)

-}
baz = \f p s -> fmap (first f) $ runParser p s

{-
  > runParser (fmap ord (char 'a')) "abc"
  Just (97,"bc")
-}
instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   -- fmap f p = Parser { runParser = \s -> fmap (first f) $ runParser p s }
   fmap f p = wrap $ \s -> fmap (first f) $ (unwrap p) s


instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = wrap (\s -> Just (a, s))
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf pa =
         let
            inner = \s -> case (unwrap pf) s of
                          Nothing -> Nothing
                          Just (f, s') -> (unwrap $ fmap f pa) s'
         in
           wrap inner


{-

  > :t pura ord
  pura ord :: Parser (Char -> Int)

  > :t unwrap $ pura ord
  unwrap $ pura ord :: String -> Maybe (Char -> Int, String)

-}
unwrap :: Parser a -> (String -> Maybe (a, String))
unwrap p = runParser p

{-

  > :t wrap $ unwrap $ pura ord
  wrap $ unwrap $ pura ord :: Parser (Char -> Int)

-}
wrap :: (String -> Maybe (a, String)) -> Parser a
wrap p = Parser {runParser = p}



{-

  > :t pura "foo"
  pura "foo" :: Parser [Char]

  > :t pura ord
  pura ord :: Parser (Char -> Int)

  > :t (runParser (pura ord))
  (runParser (pura ord)) :: String -> Maybe (Char -> Int, String)

-}
pura :: a -> Parser a
pura a = Parser { runParser = \s -> Just (a, s) }


first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)


-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
