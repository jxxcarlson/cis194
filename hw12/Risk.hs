{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Use: stack install MonadRandom

-- https://gist.github.com/ijt/1258156
-- https://hackage.haskell.org/package/MonadRandom

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }



threeInts :: Rand StdGen (Int, Int, Int)
threeInts =
getRandom >>= \i1 ->
getRandom >>= \i2 ->
getRandom >>= \i3 ->
return (i1,i2,i3)