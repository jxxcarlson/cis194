module HW05.Calc where

import HW05.ExprT
import HW05.Parser



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
        
