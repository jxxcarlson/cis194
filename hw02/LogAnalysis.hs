{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- TREE MANAGEMENT

{-

   data MessageTree = Leaf
                     | Node MessageTree LogMessage MessageTree
        deriving (Show, Eq)
-}
insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert p@(LogMessage _ ts' _) (Node left q@(LogMessage _ ts _) right) =
  if ts' > ts then
    Node left q (insert p right)
  else
    Node (insert p left) q right
insert (Unknown _) tree = tree



-- PARSER

{-
> parse testData
  [  LogMessage Info 1098 "ferrets! Where CAN I have dropped them, I wonder?' Alice guessed in a"
    , LogMessage Warning 3883 "Will you, won't you, will you, won't you, won't you join the dance?"
    , LogMessage (Error 9) 0 "1501 i91d900 (achDocaterfaut/input"
  ]

-}
parse :: String -> [LogMessage]
parse str =
  fmap logMessage (lines str)

logMessage :: String -> LogMessage
logMessage cs =
  let
    (msg, cs1) = messageType cs
    (ts, cs2) = int cs1
  in
    LogMessage msg ts (trim cs2)

trim :: String -> String
trim [] = []
trim (' ':cs) = trim cs
trim cs = cs


messageType :: String -> (MessageType, String)
messageType ('I':(' ':cs)) = (Info, cs)
messageType ('W':(' ':cs)) = (Warning, cs)
messageType ('E':(' ':cs)) = let (k, cs') = int cs in (Error k, cs')
messageType _ = (Error (-1), "")


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
digits (a:cs) = ([],a:cs)



-- TEST STRINGS

testData :: String
testData  = i ++ "\n" ++ w ++ "\n" ++ e

w :: String
w = "W 3883 Will you, won't you, will you, won't you, won't you join the dance?"

i :: String
i = "I 1098 ferrets! Where CAN I have dropped them, I wonder?' Alice guessed in a"

e :: String
e = "E 9 1501 i91d900 (achDocaterfaut/input"
