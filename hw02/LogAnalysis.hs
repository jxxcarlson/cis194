{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log




messageType :: String -> (MessageType, String)
messageType ('I':(' ':cs)) = (Info, cs)
messageType ('W':(' ':cs)) = (Warning, cs)
messageType ('E':(' ':cs)) = let (k, cs') = int cs in (Error k, cs')
messageType _ = (Error (-1), "")
-- messageType _ = Error 0

int :: String -> (Int, String)
int = (\(ns, cs) -> (evalDigits ns, cs)) . digits

evalDigits :: [Int] -> Int
evalDigits = evalDigits_ . reverse

evalDigits_ :: [Int] -> Int
evalDigits_ [] = 0
evalDigits_ (n:[]) = n
evalDigits_ (n:ns) = n + 10*evalDigits_ ns

digits :: String -> ([Int], String)
digits ('0':cs) = let (ns,cs_) = digits cs in ((0:ns),cs_)
digits ('1':cs) = let (ns,cs_) = digits cs in ((1:ns),cs_)
digits ('2':cs) = let (ns,cs_) = digits cs in ((2:ns),cs_)
digits ('3':cs) = let (ns,cs_) = digits cs in ((3:ns),cs_)
digits ('4':cs) = let (ns,cs_) = digits cs in ((4:ns),cs_)
digits ('5':cs) = let (ns,cs_) = digits cs in ((5:ns),cs_)
digits ('6':cs) = let (ns,cs_) = digits cs in ((6:ns),cs_)
digits ('7':cs) = let (ns,cs_) = digits cs in ((7:ns),cs_)
digits ('8':cs) = let (ns,cs_) = digits cs in ((8:ns),cs_)
digits ('9':cs) = let (ns,cs_) = digits cs in ((9:ns),cs_)
digits (_:cs) = ([],cs)
digits _ = ([],"")



-- TEST STRINGS

w = "W 3883 Will you, won't you, will you, won't you, won't you join the dance?"
i = "I 1098 ferrets! Where CAN I have dropped them, I wonder?' Alice guessed in a"
e = "E 9 1501 i91d900 (achDocaterfaut/input"
