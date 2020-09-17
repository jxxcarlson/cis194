module AParser () where

import           Control.Applicative

import           Data.Char

{-

  > ap =  fmap (\c -> (\_ -> c)) (char 'a')
  > bp = char 'b'
  > abp = ap <*> bp

  > :t ap
  ap :: Parser (p -> Char)

  > :t bp
  bp :: Parser Char

> :t abp
  abp :: Parser Char


  > runParser abp "abxyz"
  Just ('a',"xyz")

  > runParser abp "baxyz"
  Nothing

  -- EXERCISE 3a --

  > ap = fmap (\c -> (\c' -> (c, 'b'))) (char 'a')
  > abp = ap <*> bp

  > :t abp
  abp :: Parser (Char, Char)

  >
  Just (('a','b'),"cd")

  > runParser abp "xbcd"
  Nothing

  > runParser abp "axcd"
  Nothing

  -- EXERCISE 3b --

  > abp_ = fmap (\(x,y) -> ()) abp
  > :t abp_
  abp_ :: Parser ()
  > runParser abp_ "abcd"
  Just ((),"cd")
  > runParser abp_ "bacd"
  Nothing


  --- EXERCISE 3c ---



-}
abp :: Parser (Char, Char)
abp = ap <*> bp



{-
  > runParser (posInt' <*> space) "12 ab"
  Just (12,"ab")
-}
posInt' = fmap (\n ->  (\n_ -> n)) posInt

posInt'' = fmap (\n ->  (\n_ -> n:[n_] )) posInt

space = char ' '

space' = fmap (\n -> (\n_ -> n_)) space

{-

  > runParser  (posInt'' <*> (space' <*> posInt)) "12 34 abc"
  Just ([12,34]," abc")

-}


{-

> bp = char 'b'

> ap = fmap (\c -> (\c' -> (c, 'b'))) (char 'a')
> runParser (ap <*> bp) "abcd"
Just (('a','b'),"cd")

-}
ap = fmap (\c -> (\c' -> (c, 'b'))) (char 'a')



bp = char 'b'

{-
  > runParser (fmap ord (char 'a')) "abc"
  Just (97,"bc")


-}
instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   -- fmap f p = Parser { runParser = \s -> fmap (first f) $ runParser p s }
   fmap f p = wrap $ \s -> fmap (first f) $ (unwrap p) s

{-

  > :t pure ord <*> char 'a'
  pure ord <*> char 'a' :: Parser Int

  > runParser (pure ord <*> char 'a') "abc"
  Just (97,"bc")

  > runParser (pure ord <*> char 'a') "def"
  Nothing

-}
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



--- HELPERS ---

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


--- SCRATH WORK ---

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

  > :t pura "foo"
  pura "foo" :: Parser [Char]

  > :t pura ord
  pura ord :: Parser (Char -> Int)

  > :t (runParser (pura ord))
  (runParser (pura ord)) :: String -> Maybe (Char -> Int, String)

-}
pura :: a -> Parser a
pura a = Parser { runParser = \s -> Just (a, s) }
