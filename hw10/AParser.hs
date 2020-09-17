module AParser () where

import  Control.Applicative

import  Data.Char

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

{-
  > runParser (fmap ord (char 'a')) "abc"
  Just (97,"bc")

-}
instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap f p = Parser { runParser = \s -> fmap (first f) $ runParser p s }

{-

  > :t pure ord <*> char 'a'
  pure ord <*> char 'a' :: Parser Int

  > runParser (pure ord <*> char 'a') "abc"
  Just (97,"bc")

  > runParser (pure ord <*> char 'a') "def"
  Nothing

-}
instance Applicative Parser where
   pure a = Parser (\s -> Just (a, s))
   p1 <*> p2 = Parser (\s ->
         case runParser p1 s of
            Nothing -> Nothing
            Just (f, s') -> runParser (f <$> p2) s'
      )


{-

  > runParser ((char 'a') <|> (char 'b')) "axy"
  Just ('a',"xy")

  > runParser ((char 'a') <|> (char 'b')) "bxy"
  Just ('b',"xy")

-}


instance Alternative Parser where
  empty = Parser (\s -> Nothing )
  pa <|> pb = Parser (\s ->
            case runParser pa s of
              Just v -> Just v
              Nothing ->
                runParser pb s
          )

--- HELPERS ---


first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)


-- PUR --

{-

  > :t pur ord
  pur ord :: Parser (Char -> Int)

  > :t unwrap $ pur ord
  unwrap $ pur ord :: String -> Maybe (Char -> Int, String)

-}
pur :: a -> Parser a
pur a = Parser { runParser = \s -> Just (a, s) }


--- EXERCISE 3 ---

ap :: Parser (p -> (Char, Char))
ap = fmap (\c -> (\_ -> (c, 'b'))) (char 'a')

bp :: Parser Char
bp = char 'b'

abp :: Parser (Char, Char)
abp = ap <*> bp

-- NOTE: Parser (p -> (Char, Char)) -> Parser Char -> Parser (Char, Char)

{-
    > runParser abp "abxyz"
    Just (('a','b'),"xyz")
-}

abp_ :: Parser ()
abp_ = fmap (\(x,y) -> ()) abp

{-

  > runParser abp_ "abxyz"
  Just ((),"xyz")

-}


posInt' = fmap (\n ->  (\n_ -> n)) posInt

posInt'' = fmap (\n ->  (\n_ -> n:[n_] )) posInt

space = char ' '

space' = fmap (\n -> (\n_ -> n_)) space

intPair :: Parser [Integer]
intPair = posInt'' <*> (space' <*> posInt)

{-

  > runParser intPair "12 34xyz"
  Just ([12,34],"xyz")

-}

--- EXERCISE 5 ---

intOrUppercase :: Parser ()
intOrUppercase = (fmap (\x -> ()) posInt) <|> (fmap (\c -> ()) (satisfy isUpper))

{-

  > runParser intOrUppercase "12abc"
  Just ((),"abc")

  > runParser intOrUppercase "Xabc"
  Just ((),"abc")

  > runParser intOrUppercase "xabc"
  Nothing

-}
