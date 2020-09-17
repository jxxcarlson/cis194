module Exercises()


import AParser


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
