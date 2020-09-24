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

threeInts :: Rand StdGen (Int, Int, Int)
threeInts =
  getRandom >>= \i1 ->
  getRandom >>= \i2 ->
  getRandom >>= \i3 ->
  return (i1,i2,i3)


-- > evalRandIO $ rollDie 4
--   [DV {unDV = 6},DV {unDV = 4},DV {unDV = 5},DV {unDV = 2}]
dieRolls :: Int -> Rand StdGen [DieValue]
dieRolls n = sequence $ replicate n die

sortedDieRolls :: Int -> Rand StdGen [DieValue]
sortedDieRolls n = (reverse . sort) <$> dieRolls n

battles :: Int -> Int -> Rand StdGen [(DieValue, DieValue)]
battles attackers defenders = 
  let 
    n = min attackers defenders
  in
    zip <$> sortedDieRolls n <*> sortedDieRolls n

battleOutcome :: (DieValue, DieValue)  -> BattleOutcome
battleOutcome (attackersDie, defendersDie) = 
  if attackersDie > defendersDie 
    then AttackerWins
    else DefenderWins

-- > evalRandIO $ battleOutcomes 3 5
--   [DefenderWins,DefenderWins,DefenderWins]
battleOutcomes :: Int -> Int -> Rand StdGen [BattleOutcome]
battleOutcomes attackers defenders = 
  map battleOutcome <$> battles attackers defenders


incrementBattlefield :: Int -> Int -> Battlefield -> Battlefield
incrementBattlefield i j b = 
  Battlefield { attackers = attackers  b + i, defenders = defenders b + j }

decrementBattlefield :: Int -> Int -> Battlefield -> Battlefield
decrementBattlefield i j b = 
  Battlefield { attackers = attackers  b - i, defenders = defenders b - j }

b :: Battlefield
b = Battlefield { attackers = 5, defenders = 6 }


-- > evalRandIO $ battle b
--   Battlefield {attackers = 3, defenders = 5}
battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = 
  let 
    attackers_ = attackers battlefield
    defenders_ = defenders battlefield
    actualAttackers = min 3 (attackers_ - 1)
    actualDefenders = min 2 defenders_
    genResults = battleOutcomes actualAttackers actualDefenders
    successfulAttacks = length <$> filter (\x -> x == AttackerWins) <$> genResults 
    successfulDefenses = length <$> filter (\x -> x == DefenderWins) <$> genResults 
  in 
    decrementBattlefield <$> successfulDefenses <*> successfulAttacks <*> pure battlefield



-- NOTES:
-- evalRandIO $ threeInts :: IO (Int, Int, Int)
--
-- > evalRandIO $ threeInts :: IO (Int, Int, Int)
--   (-1855278204539649463,-3128923297145675342,6881384312351252373)
--
-- > :t print
-- print :: Show a => a -> IO ()
--
-- :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b



main = do
    rolls <- evalRandIO $ threeInts
    print rolls  


--- HELPERS ---

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 h fa fb = h <$> fa <*> fb