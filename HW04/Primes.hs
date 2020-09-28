module HW03.Primes where

-- Sieve of Sundaram (https://en.wikipedia.org/wiki/Sieve_of_Sundaram)

import Data.List (sort)
import Data.List.Ordered (member)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

uniq :: Ord a => [a] -> [a]
uniq as = 
    foldr (\a acc -> if member a acc then acc else a:acc) [] as
 
composites :: Integer -> [Integer]
composites n = 
 uniq . sort . filter (\i -> i <= n) 
   . map (\(a,b) -> a + b + 2*a*b) 
   $  cartProd [1..n] [1..n]

-- > primes 50
-- [3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101]
primes :: Integer -> [Integer]
primes n = 
  let 
    cs = composites n
  in
    map (\i -> 2*i + 1) $ (filter (\i -> not $ member i cs)) [1..n]