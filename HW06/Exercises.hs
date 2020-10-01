module HW06.Exercises where

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


new :: Num a => [a] -> [a]
new [] = [0]
new [x] = [1,x]
new (x:y:xs) = x + y : (x:y:xs)

iter :: Num a => Int -> (a -> a) -> a -> a
iter 0 _ a = a
iter n f a = iter (n - 1) f (f a)

-- fibs2 :: [Integer]
-- -- fibs2 = foldr (\_ (x:y:z) -> (x + y):x:y:z) [1,0] [1..]
-- fibs2 = foldl (\(x:y:z) _ -> (x + y):x:y:z) [1,0] [1..]

fibs2 :: Int -> [Integer]
fibs2 n = foldl addFib [0,1] [1..n]

fibs2' :: [Integer]
fibs2' = foldl addFib [0,1] [1..]

addFib :: Num a => [a] -> Int -> [a]
addFib fs n = 
    fs ++ [fs !! n + fs !! (n - 1)]
    