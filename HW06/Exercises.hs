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


-- Exercise 2

fibs2 :: Int -> [Integer]
fibs2 n = foldl addFib [0,1] [1..n]

fibs2' :: [Integer]
fibs2' = foldl addFib [0,1] [1..]

addFib :: Num a => [a] -> Int -> [a]
addFib fs n = 
    fs ++ [fs !! n + fs !! (n - 1)]
    

-- Exercise 3

data Stream a = Cons a  (Stream a)

takeOne :: Stream a -> (a, Stream a)
takeOne (Cons a s) = (a, s)

streamToList :: Stream a -> [a]
streamToList s =
    let 
        (a, s') = takeOne s
    in 
        a :  streamToList s'

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = 
    Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a =
    Cons a (streamFromSeed f (f a))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

ff :: Integer -> Integer
ff x
  | x == 0       = 0
  | odd x        = 0
  | even x       = 1 + ff (x `div` 2)


ruler :: Stream Integer
ruler = streamMap ff nats