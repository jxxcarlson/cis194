module HW05.Calc2 where

import HW05.ExprT
import HW05.Parser ( parseExp )
import HW05.StackVM



-- > eval  (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
--   20
eval :: ExprT -> Integer
eval expr = 
    case expr of 
        Lit k -> k
        Add e f -> eval e + eval f 
        Mul e f -> eval e * eval f

-- > evalExp  "2+3*4"
--   Just 14
-- > evalExp  "2+3*"
--   Nothing
evalExp :: String -> Maybe Integer
evalExp str = eval <$> parseExp Lit Add Mul str

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


instance Expr ExprT where 
    lit k = Lit k
    add e f = Add e f
    mul e f = Mul e f


instance Expr Integer where 
    lit k = k
    add e f = e + f
    mul e f = e * f





(+!) :: Mod7 -> Mod7 -> Mod7
(+!) (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)

(*!) :: Mod7 -> Mod7 -> Mod7
(*!) (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
 
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7



-- Example:
-- > eval $ add (lit 2) (lit 3)
-- 5    

-- > :t reify $ add (lit 2) (lit 3)
--   reify $ add (lit 2) (lit 3) :: ExprT
--
-- > reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)
reify :: ExprT -> ExprT
reify = id


