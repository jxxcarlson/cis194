

-- > toDigits 1234
-- [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigits_ n)

toDigits_ :: Integer -> [Integer]
toDigits_ n
  | n == 0        = []
  | otherwise     = n `mod` 10 : toDigits_ (n `div` 10)

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs


addEvens :: [Integer] -> Integer
addEvens [] = 0
addEvens (x:[]) = x
addEvens (x:(y:[])) = x
addEvens (x1:(x2:xs)) = x1 + addEvens xs

addOdds :: [Integer] -> Integer
addOdds xs = addEvens (drop 1 xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = (2*x:[])
doubleEveryOther (x:(y:[])) = (2*x:(y:[]))
doubleEveryOther (x:(y:xs)) = (2*x:y:doubleEveryOther xs)

sumDigits :: [Integer] -> Integer
sumDigits ns = (sum . concat . (map toDigits)) ns

checkSum :: Integer -> Integer
checkSum n =
  ((sumDigits . doubleEveryOther . toDigits) n) `mod` 10

checkSum1 :: Integer -> Integer
checkSum1 n =
  let
    ns = toDigits n
    s = 2 * addEvens ns + addOdds ns
  in
    s `mod` 10

validate :: Integer -> Bool
validate n =
  checkSum n == 0


-- XXX ---

doubleEveryOtherRR :: [Integer] -> [Integer]
doubleEveryOtherRR []       = []
doubleEveryOtherRR (x:[])   = x:[]
doubleEveryOtherRR (x:y:xs) = x:(2*y):doubleEveryOtherRR xs