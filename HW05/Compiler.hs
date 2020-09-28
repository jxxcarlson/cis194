{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module HW05.Calc2 where

--import HW05.ExprT
import HW05.Parser ( parseExp )
import HW05.StackVM


class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


instance Expr Program where 
    lit k = [PushI k]
    add e f = f ++ e ++ [Add]
    mul e f = f ++ e ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

input1 :: [Char]
input1 = "(3 * -4) + 5"

run :: String -> Maybe (Either String StackVal)
run input = 
    stackVM <$> compile input