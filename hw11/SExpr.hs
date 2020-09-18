{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
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

parse :: String -> Maybe SExpr
parse source =
  fst <$> runParser sexpr source

-- Determine if a string representing an S-expression is valid
valid :: String -> Bool
valid source =
   case runParser sexpr source of
      Nothing -> False
      Just (_, residual) -> residual == ""


data Value = Num Integer | Str String  deriving Show

evalAst :: SExpr -> Maybe Value 
evalAst sexpr =
  case sexpr of
    A atom ->
      case atom of 
        N k -> Just $ Num k
        I str -> Just $ Str str
    Comb list -> 
      case head list of
        A (I "add") -> Nothing  -- add $ map evalAst (tail list)
        _ -> Nothing  

-- valueToInt :: Value -> Maybe Integer
-- valueToInt value = 
--     case value of
--       Num k -> Just k 
--       Str _ -> Nothing 

-- add :: [Maybe Value] -> Maybe Value 
-- add values = undefined
--   (sum <$> (map valueToInt)) values
  

-- eval :: String -> Value 
-- eval str =
--   case parse str of
--     Nothing -> Undefined
--     Just ast ->  evalAst ast