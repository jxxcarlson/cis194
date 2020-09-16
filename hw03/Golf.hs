module Golf where

skips :: [a] -> [[a]]
skips items =
  foldr (\k acc -> (skip k items):acc) [] [2..((length items + 1) `div` 2 )]

skip :: Int -> [a] -> [a]
skip k items =
  drop 1 $ map snd $ filter (\(i,_) -> i `mod` k == 0) (zip [0..(length items)] items)
