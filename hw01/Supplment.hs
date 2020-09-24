


-- Pattern matching
initials :: [Char] -> [Char] -> [Char]
initials (a:_) (b:_) = a : '.' : b : '.' : []
initials (a:_) _ =  a : '.' : []
initials _ (b:_) =  b : '.' : []
initials _ _ = "XX.YY."


-- Recursion
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a : replicate' (n - 1) a
