module HW03.Tree where 

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)


height :: Tree a -> Integer
height Leaf = -1
height (Node k _ _ _) = k

height' :: Tree a -> Integer
height' Leaf = 0
height' (Node _ l _ r) = 1 + max (height l) (height r)

left :: Tree a -> Maybe (Tree a)
left (Node _ l _ _) = Just l
left Leaf = Nothing

right :: Tree a -> Maybe (Tree a)
right (Node _ _ _ r) = Just r
right Leaf = Nothing

root :: Tree a -> Maybe a
root (Node _ _ a _) = Just a
root Leaf = Nothing


insert :: a -> Tree a -> Tree a
insert a t =
    case t of
        Leaf -> Node 0 Leaf a Leaf
        (Node k left a' right) -> 
            case compare (height left) (height right) of
                LT -> Node k (insert a left) a' right
                GT -> Node k left a' (insert a right)
                EQ ->  
                    let
                        left' = (insert a left) 
                        k' = height left'
                    in   
                        Node (k' + 1) (insert a left) a' right

foldTree :: [a] -> Tree a 
foldTree as = foldr insert Leaf as

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l _ r) = 
    abs ((height l) - (height r)) <= 2 && isBalanced l && isBalanced r 
    