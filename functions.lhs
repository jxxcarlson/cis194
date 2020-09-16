
These are my notes from CIS 194

FUNCTIONS

Functions can be defined by a system of equations:

> sumtorial :: Integer -> Integer
> sumtorial 0 = 0
> sumtorial n = n + sumtorial (n - 1)

They can also be defined using guards:

> hailstone :: Integer -> Integer
> hailstone n
>  | n `mod` 2 == 0 = n `div` 2
>  | otherwise      = 3*n + 1


LISTS

Ranges

> l1 = [1..10]
> l2 = [1,3..20]
> l3 = [20,18..0]

Elements

l1 !! 0
1

A String and a list of Char are the same:

> hello1 :: [Char]
> hello1 = ['h', 'e', 'l', 'l', 'o']

> hello2 :: String
> hello2 = "hello"

> helloSame = hello1 == hello2

The notation [2,3,4] is just convenient shorthand for 2 : 3 : 4 : [].
Note also that these are really singly linked lists, NOT arrays.

-- Generate the sequence of hailstone iterations from a starting number.

> hailstoneSeq :: Integer -> [Integer]
> hailstoneSeq 1 = [1]
> hailstoneSeq n = n : hailstoneSeq (hailstone n)

We stop the hailstone sequence when we reach 1.
The hailstone sequence for a general n consists of n itself,
followed by the hailstone sequence for hailstone n,
that is, the number obtained by applying
he hailstone transformation once to n.
