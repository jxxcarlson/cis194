module HW03.Wholemeal where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs    

g1 :: Integral p => p -> p -> p
g1 acc x = if even x then (x - 2) * acc else acc 

fun1' :: [Integer] -> Integer
fun1' = foldl g1 1


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

fun2' :: Integer -> Integer
fun2' = sum . (filter even) . hailstoneSeq

test :: Integer -> Bool
test n = fun2 n == fun2' n

test' ::  Integer -> Bool
test' n = all test [1..n]