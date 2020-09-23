module Tree where

data Tree a = EmptyTree | Node (Tree a ) a (Tree a)
  deriving (Show, Eq)

-- The goal of this exercise is to understand ordered trees.
-- These have type Tree a, where there is an order
-- defined on values of type a.  Thus a could be Integer 
-- or String, for example.  Below we define the type family
-- Tree a, we define the relation

--     a !> Tree a :: Bool

-- where this means that a is greater than the root of
-- of the tree on the righ-hand side of the relation !>.
-- A tree is "in order" if its root is greater thatn the
-- left subtree and less that the right subtree, and if
-- both subtrees are in order.

-- In the code below, replace the "undefined" clauses
-- with working code.


-- Create a tree with one node 
singleton :: a -> Tree a
singleton a = undefined

-- Return left subtree
left :: Tree a -> Tree a
left (Node left_ a _right) = undefined

-- Return right subtree
right :: Tree a -> Tree a
right (Node _left a right_) = undefined

-- Return top node
top :: Tree a -> a
top EmptyTree = error "The empty tree has no nodes"
top (Node left_ a right_) = undefined

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
isInOrder (Node left_ a right_) = undefined

-- Inserting a new node into an ordered tree returns an ordered tree
insert :: (Ord a) => a -> Tree a -> Tree a
insert a EmptyTree = Node EmptyTree a EmptyTree
insert a (Node left_ b right_) = undefined


inOrder :: Ord a => Tree a ->  [a]
inOrder EmptyTree = []
inOrder t@(Node left_ a right_) = undefined


s :: Tree Integer
s = Node (singleton 1) 2 (singleton 3)

s' :: Tree Integer
s' = Node (singleton 1) 3 (singleton 2)

t :: Tree String
t = Node (singleton "a") "b" (singleton "c")

-- Test:
--
-- > isInOrder s
-- True
--
-- > isInOrder s'
-- False
--
-- > isInOrder t
--   True