{- CIS 194 HW 11
   due Monday, 8 April
-}

module HW11.SExpr where

import HW11.AParser
import Control.Applicative

import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
   let
     p' =  (\a -> [a]) <$> p
     p'' = (\a -> (\b -> a ++ b)) <$> p'
   in
     (p'' <*> (zeroOrMore p) <|> p') <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = 
  let
    p' = (\a -> [a]) <$> p
    p'' = (\a -> (\b -> a ++ b)) <$> (oneOrMore p)
  in
    p'' <*> p'


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (char ' ')

ident :: Parser String
ident = 
   let
    p' = (\a -> (\b -> a:b)) <$> (satisfy isAlpha)  
    -- Parser ([Char] -> [Char])
  in
  p' <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are vali:d identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

atom :: Parser Atom 
atom = N <$> (posInt <* spaces) <|> I <$> (ident <* spaces)

sexpr :: Parser SExpr
sexpr = parenthesizedExpression  <|> A <$> atom 

parenthesizedExpression :: Parser SExpr 
parenthesizedExpression = (leadingParen *> (Comb <$> (zeroOrMore sexpr))) <* trailingParen

leadingParen :: Parser Char
leadingParen = (char '(') <* spaces

trailingParen :: Parser Char
trailingParen = (char ')') <* spaces

parse :: String -> Maybe (SExpr, String)
parse source =
  runParser sexpr source

-- Determine if a string representing an S-expression is valid
valid :: String -> Bool
valid source =
   case parse source of
      Nothing -> False
      Just (_, residual) -> residual == ""


-- Tests:

-- > parse "(sum 1 2)"
--   Just (Comb [A (I "sum"),A (N 1),A (N 2)],"")

-- > parse "(product 1 2 (sum 3 4))"
--   Just (Comb [A (I "product"),A (N 1),A (N 2),Comb [A (I "sum"),A (N 3),A (N 4)]],"")

-- > parse "foo bar"
--   Just (A (I "foo"),"bar")

------------------------------------------------------------
--  3. Evaluating S-expressions
------------------------------------------------------------

data Value = Num Integer | Str String  | BadSExpr deriving Show

evalSExpr :: SExpr -> Maybe Value 
evalSExpr sexpr =
  case sexpr of
    A atom ->
      case atom of 
        N k -> Just $ Num k
        I str -> Just $ Str str
    Comb list -> 
      case head list of
        A (I "sum") -> 
           Num <$> sum <$> (evalIntegerArgs $ tail list)
        A (I "product") -> 
           Num <$> product <$> (evalIntegerArgs $ tail list)
        _ -> Nothing  


evalIntegerArgs :: [SExpr] -> Maybe [Integer]
evalIntegerArgs args = sequenceA $ map (valueToInteger . evalSExpr) args
 
valueToInteger :: Maybe Value -> Maybe Integer
valueToInteger value = 
    case value of
      Nothing -> Nothing 
      Just ( Num k ) -> Just k 
      Just (Str _) -> Nothing 
      Just BadSExpr -> Nothing



{-

  > eval "foo"
  Just (Str "foo")

  > eval "42"
  Just (Num 42)

  > eval "(product 2 (sum 3 4))"
  Just (Num 14)

  > eval "(foo 1 2)"
  Nothing 

  > eval "sum 1 2"
  Just BadSExpr
  
-}
eval :: String -> Maybe Value 
eval str =
  let 
    result = parse str
  in
    if (snd <$> result) == Just "" then
      fst <$> result >>= evalSExpr  
    else
      Just BadSExpr


-- JUMPING AHEAD TO CH 12 MATERIAL ON MONADS --

-- NOTE re discussion in Chapter 12 lecture notes
-- Applicative parsers can be sequenced using <*> and <$>

data Foo = Bar Int Int Ident deriving Show

parseInt :: Parser Int
parseInt = posInt_ <* spaces

parseFoo :: Parser Foo
parseFoo = Bar <$> parseInt <*> parseInt <*> ident

-- Test:
-- > runParser parseFoo "123 456 hola"
-- Just (Bar 123 456 "hola","")
--
-- > runParser parseFoo "!! 123 456 hola"
-- Nothing

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

-- Example:
-- > [1,2,3,4] >>= addOneOrTwo
-- [2,3,3,4,4,5,5,6]

--- PARSEFILE EXMPLE ---

-- replicateM :: Monad m => Int -> m a -> m [a]
-- replicateM n m = sequence (replicate n m)

-- parseFile :: Parser [[Int]]
-- parseFile = many parseLine

-- parseLine :: Parser [Int]
-- parseLine = parseInt >>= \i -> replicateM i parseInt