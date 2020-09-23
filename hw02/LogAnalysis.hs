{-# OPTIONS_GHC -Wall #-}

module HW02.LogAnalysis where

import HW02.Log

testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse_ n file = take n . parse_ <$> readFile file


{-

  *LogAnalysis> testParse (badStuff 50) 10 "error.log"
  [  LogMessage (Error 55) 131 "Mustardwatch opened, please close for proper functioning!"
   , LogMessage (Error 76) 570 "All backup mustardwatches are busy"
   , LogMessage (Error 88) 1302 "Depletion of mustard stores detected!"
   , LogMessage (Error 91) 1891 "Hard drive failure: insufficient mustard"
   , LogMessage (Error 50) 3316 "All backup mustardwatches are busy"
   , LogMessage (Error 99) 4219 "Twenty seconds remaining until out-of-mustard condition"
   , LogMessage (Error 88) 4425 "Ten seconds remaining until out-of-mustard condition"
   , LogMessage (Error 83) 5366 "Empty mustard reservoir! Attempting to recover..."
   , LogMessage (Error 75) 5395 "Recovery failed! Initiating shutdown sequence"]

-}
badStuff :: Int -> String -> [LogMessage]
badStuff minimumBadness str =
  let
     predicate :: LogMessage -> Bool
     predicate (LogMessage (Error b) _ _) = b >= minimumBadness
     predicate _ = False
  in
     filter predicate (sortLog str)

{-

  > testData
  "I 1098 ferrets! Where CAN I have dropped them, I wonder?' Alice guessed in a\n
   W 3883 Will you, won't you, will you, won't you, won't you join the dance?\n
  E 9 1501 i91d900 (achDocaterfaut/input"

  > sortLog testData
    [ LogMessage Info 1098 "ferrets! Where CAN I have dropped them, I wonder?' Alice guessed in a"
    , LogMessage (Error 9) 1501 "i91d900 (achDocaterfaut/input"
    , LogMessage Warning 3883 "Will you, won't you, will you, won't you, won't you join the dance?"]
-}
sortLog :: String -> [LogMessage]
sortLog = inOrder . build . parse

-- TREE MANAGEMENT

-- Convert a MessageTree to an ordered list of LogMessage.  
-- The definition below relies of the fact that elements of
-- the left tree are greater than elements of the the right tree.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left n right) =
  inOrder left ++ (n: inOrder right)

-- Recursively build a MessageTree from a list of LogMessage.  
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm: lms) = insert lm (build lms)

-- Recall the definition of MessageTree
--
--     data MessageTree = Leaf
--                        | Node MessageTree LogMessage MessageTree
--        deriving (Show, Eq)
--
-- FROME THE TEXT:
-- A MessageTree should be sorted by timestamp: that is, the timestamp 
-- of a LogMessage in any Node should be greater than all timestamps 
-- of any LogMessage in the left subtree, and less than all timestamps 
-- of any LogMessage in the right child
--
-- Let p be the LogMessage to insert and let the tree into
-- which it is inserted be 
--
--         Node left q right
--
-- If the timestamp of p is greater than the time stamp of q,
-- insert p in the right subtree.  Otherwise, insert it in the
-- left subtree.
--
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
messageType ('I':(' ':cs)) = (Info, (trim cs))
messageType ('W':(' ':cs)) = (Warning, (trim cs))
messageType ('E':(' ':cs)) = let (k, cs') = int (trim cs) in (Error k, (trim cs'))
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
digits _ = ([], "")



-- TEST STRINGS

testData :: String
testData  = i ++ "\n" ++ w ++ "\n" ++ e

w :: String
w = "W 3883 Will you, won't you, will you, won't you, won't you join the dance?"

i :: String
i = "I 1098 ferrets! Where CAN I have dropped them, I wonder?' Alice guessed in a"

e :: String
e = "E 9 1501 i91d900 (achDocaterfaut/input"
