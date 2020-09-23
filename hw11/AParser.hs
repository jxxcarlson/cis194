module HW11.AParser (Parser, runParser, satisfy, char, posInt, posInt_) where

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

posInt_ :: Parser Int
posInt_ = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs      

inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first

instance Applicative Parser where
   pure a = Parser (\s -> Just (a, s))
   p1 <*> p2 = Parser (\s ->
         case runParser p1 s of
            Nothing -> Nothing
            Just (f, s') -> runParser (f <$> p2) s'
      )


instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

