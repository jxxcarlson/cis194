module Golf where

skips :: [a] -> [[a]]
skips items =
  foldr (\k acc -> (skip k items):acc) [] [2..((length items + 1) `div` 2 )]

skip :: Int -> [a] -> [a]
skip k items =
  drop 1 $ map snd $ filter (\(i,_) -> i `mod` k == 0) (zip [0..(length items)] items)

{-

  > localMaxima [1, 2, 4, 2, 3, 5, 4, 1]
  [4,5]

  > localMaxima [1, 2, 3, 4, 5]
  []

-}
localMaxima :: [Integer] -> [Integer]
localMaxima ns =
  fmap (\(a_, b_, c_) -> b_) $ filter (\(a, b, c) -> b > a && b > c)
    (zip3 (head ns : take (length ns - 1) ns) ns (drop 1 ns ++ [last ns]))
