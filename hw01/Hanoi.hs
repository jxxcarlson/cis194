

type Peg = String
type Move = (String, String)

hanoi_ :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi_ 0 a b c = []
hanoi_ 1 a b c = [(a,c)]
hanoi_ n a b c = hanoi (n - 1) c a b ++  hanoi 1 a c b ++ hanoi ( n - 1 ) a b c

hanoi n a b c = reverse (hanoi_ n a b c)
