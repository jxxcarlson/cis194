module Tree where

data Tree a = EmptyTree | Node (Tree a ) a (Tree a)
  deriving (Show, Eq)


-- If a is of typeclass Ord, then we can speak of a tree as
-- being ordered.  
--   

-- Create tree with one node 
singleton :: a -> Tree a
singleton a = Node EmptyTree a EmptyTree

-- Return left subtree
left :: Tree a -> Tree a
left (Node left_ _ _) = left_

-- Return right subtree
right :: Tree a -> Tree a
right (Node _ _ right_) = right_

-- Return top node
top :: Tree a -> a
top EmptyTree = error "The empty tree has no nodes"
top (Node left_ a right_) = a

-- > t = Node (singleton 1) 2 (singleton 3)
-- > 4 !> t
--   True
(!>) :: Ord a => a -> Tree a -> Bool
_ !> EmptyTree = True
a !> (Node _ b _) = a > b

-- > t = Node (singleton 1) 2 (singleton 3)
-- > 0 <=! t
--   True
(<=!) :: Ord a => a -> Tree a -> Bool
_ <=! EmptyTree = True
a <=! (Node _ b _) = a <= b


-- > isInOrder $ Node (singleton 1) 2 (singleton 3)
--   True
--
-- > isInOrder $ Node (singleton 1) 2 (singleton 1)
--   False
-- 
isInOrder ::  Ord a => Tree a -> Bool
isInOrder EmptyTree = True
isInOrder (Node EmptyTree _ EmptyTree) = True
isInOrder (Node left_ a right_) = 
  a !> left_ && a <=! right_ && isInOrder left_ && isInOrder right_

-- Inserting a new node into an ordered tree returns an ordered tree
insert :: (Ord a) => a -> Tree a -> Tree a
insert a EmptyTree = Node EmptyTree a EmptyTree
insert a (Node left_ b right_) = 
  if a > b 
    then
     (Node left_ b (insert a right_))
    else
      (Node (insert a left_) b right_)


inOrder :: Ord a => Tree a ->  [a]
inOrder EmptyTree = []
inOrder t@(Node left_ a right_) =  
  if isInOrder t 
    then inOrder left_ ++  a:(inOrder right_)
    else error "Tree is not in order"


s :: Tree Integer
s = Node (singleton 1) 2 (singleton 3)

s' :: Tree Integer
s' = Node (singleton 1) 3 (singleton 2)

-- Test:
--
-- isInOrder s
-- True
--
-- isInOrder s'
-- False