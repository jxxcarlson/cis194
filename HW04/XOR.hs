module HW03.XOR where

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

xor' :: [Bool] -> Bool
xor' bs = 
    foldr (\b acc -> xor b acc) False bs


-- > map' (\x -> 2*x) [1,2,3,4]
-- [2,4,6,8]
map' :: (a -> b) -> [a] -> [b]
map' f as = foldr (\a acc -> f a : acc) [] as