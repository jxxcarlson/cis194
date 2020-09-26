module HW12.Birds where

type Birds = Int
type Pole = (Birds, Birds)

empty :: (Birds, Birds)
empty = (0,0)

(|>) :: t1 -> (t1 -> t2) -> t2
x |> f = f x

landLeft' :: Birds -> Pole -> Pole  
landLeft' n (left,right) = (left + n,right)  
  
landRight' :: Birds -> Pole -> Pole  
landRight' n (left,right) = (left,right + n)

-- Example:
-- > empty |> landLeft' 2 |> landRight' 3
-- (2,3)

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing


-- Example:
-- > empty |> landLeft 2 >>= landRight 3
--   Just (2,3)
-- > empty |> landLeft 2 >>= landRight 3 >>= landLeft 20
--   Nothing

main :: Maybe Int -> Maybe Int -> Maybe Int
main a b = 
    do 
     a' <- a 
     b' <- b 
     Just $ a' + b'
 