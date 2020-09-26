{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Use: stack install MonadRandom

-- https://gist.github.com/ijt/1258156
-- https://hackage.haskell.org/package/MonadRandom
-- Notes on random number generation: https://www.seas.upenn.edu/~cis552/12fa/lectures/stub/RandomGen.html


module HW12.Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)


-- Apply f to the first element of the tuple
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

data BattleOutcome = AttackerWins | DefenderWins deriving (Show, Eq)


-- TEST DATA
b :: Battlefield
b = Battlefield { attackers = 5, defenders = 5 }

b':: Battlefield
b' = Battlefield { attackers = 3, defenders = 3 }

bb :: Battlefield
bb = Battlefield { attackers = 100, defenders = 100 }


-- DICE

-- > evalRandIO $ rollDie 4
--   [DV {unDV = 6},DV {unDV = 4},DV {unDV = 5},DV {unDV = 2}]
dieRolls :: Int -> Rand StdGen [DieValue]
dieRolls n = sequence $ replicate n die


-- > evalRandIO $ sortedDieRolls 4
--   [DV {unDV = 6},DV {unDV = 5},DV {unDV = 4},DV {unDV = 3}]
sortedDieRolls :: Int -> Rand StdGen [DieValue]
sortedDieRolls n = (reverse . sort) <$> dieRolls n

--  PURPOSE: produces a generator for n pairs of sorted die rolls
--  > evalRandIO $ battles 3 3
--    [(DV {unDV = 4},DV {unDV = 4}),(DV {unDV = 3},DV {unDV = 2}),(DV {unDV = 2},DV {unDV = 2})]
genPairs :: Int -> Rand StdGen [(DieValue, DieValue)]
genPairs n = 
    zip <$> sortedDieRolls n <*> sortedDieRolls n




-- BATTLES

--  Determine the battle outcome for a give pair of die rolls
-- > battleOutcome (DV {unDV = 4},DV {unDV = 4})
--   DefenderWins
battleOutcome :: (DieValue, DieValue)  -> BattleOutcome
battleOutcome (attackersDie, defendersDie) = 
  if attackersDie > defendersDie 
    then AttackerWins
    else DefenderWins


-- Generate n random battle o
-- > evalRandIO $ battleOutcomes 3 5
--   [DefenderWins,DefenderWins,DefenderWins]
battleOutcomes :: Int -> Rand StdGen [BattleOutcome]
battleOutcomes n = 
  map battleOutcome <$> genPairs n


--  Decrement the number of attackers and defenders on the battlefield
--   If  b = Battlefield {attackers = 5, defenders = 6}, then
-- > decrementBattlefield 1 1 b
--   Battlefield {attackers = 4, defenders = 5}
decrementBattlefield :: Int -> Int -> Battlefield -> Battlefield
decrementBattlefield di dj b = 
  Battlefield { attackers = attackers  b - di, defenders = defenders b - dj }


-- Compute the number of times to roll the dice
-- given the current battlefield
numberOfBattles :: Battlefield -> Int
numberOfBattles battlefield =
  let 
    attackers_ = attackers battlefield
    defenders_ = defenders battlefield
    actualAttackers = max 0 $ min 3 (attackers_ - 1)
    actualDefenders = min 2 defenders_
  in
    min actualAttackers actualDefenders


-- Generate outcomes for random battles given the current battlefield
--  > evalRandIO $ genBattleOutcomes b
--    [AttackerWins,AttackerWins]
genBattleOutcomes :: Battlefield -> Rand StdGen [BattleOutcome]
genBattleOutcomes battlefield = 
    battleOutcomes $ numberOfBattles battlefield


-- Do battle with the provided outcomes:
doBattle :: Battlefield -> [BattleOutcome] -> Battlefield
doBattle battlefield outcomes = 
  let 
    successfulAttacks = length $ filter (\x -> x == AttackerWins)  outcomes 
    successfulDefenses = length $ filter (\x -> x == DefenderWins) outcomes 
  in 
    decrementBattlefield successfulDefenses successfulAttacks battlefield


-- Generate a new battlefeield:
--
--   - roll the dice, sort and pair them
--   - determine the outcome of each battle
--   - carry out the battles
--
-- > evalRandIO $ battle b
-- Battlefield {attackers = 3, defenders = 5}
--
-- > evalRandIO $ battle b
-- Battlefield {attackers = 4, defenders = 4}
--
-- > evalRandIO $ battle b
-- Battlefield {attackers = 3, defenders = 5}
--
battle :: Battlefield -> Rand StdGen Battlefield
battle b = doBattle b <$> (genBattleOutcomes b)


--- EXERCISE 3: INVASIONS ---


invade_ :: Rand StdGen Battlefield -> Rand StdGen Battlefield
invade_ b =
  do
    b' <- b
    if attackers b' < 2 || defenders b' == 2
      then b
      else invade_ (b >>= battle)

-- Let
--
--   b = Battlefield {attackers = 5, defenders = 5}
--
-- THIS LOOKS OK:
-- > evalRandIO $ invade b 
--   Battlefield {attackers = 1, defenders = 4}
--
-- BUT THE BELOW IS WRONG! (Invasion stops too early)
-- > evalRandIO $ invade b
-- Battlefield {attackers = 2, defenders = 3}
invade :: Battlefield -> Rand StdGen Battlefield
invade b = invade_ $ pure b

--- EXERCISE 4 ---

invadeN :: Int -> Battlefield -> Rand StdGen [Battlefield]
invadeN n b = replicate n <$> invade b


successfulAttacks :: Int -> Battlefield -> Rand StdGen Int
successfulAttacks n b = length <$> filter attackerWon <$> invadeN n b


attackerWon :: Battlefield -> Bool 
attackerWon b = defenders b == 0




-- main b = 
--   do
--     putStrLn "\nBattlefield:"
--     print b
--     out <- evalRandIO $ genBattleOutcomes b
--     putStrLn "\nBattle outcomes:"
--     print out
--     putStrLn ""
--     evalRandIO $ invade b 
--     putStrLn ""


-- main b = 
--   let 
--     outcomes = genBattleOutcomes b
--     finalBattlefield = doBattle b outcomes
--   in
--   do
--     putStrLn "\nBattlefield before:"
--     print b
--     out <- evalRandIO $ outcomes
--     putStrLn "\nBattle outcomes:"
--     print out
--     putStrLn ""
--     putStrLn "\nBattlefield after"
--     fb <- evalRandIO $ finalBattlefield
--     print fb
--     putStrLn "" 



--- HELPERS ---

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 h fa fb = h <$> fa <*> fb