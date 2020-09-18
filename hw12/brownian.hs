import Control.Monad.Random

step :: RandomGen g => Rand g Double
step = getRandomR (-1,1)

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 h fa fb = h <$> fa <*> fb

step2D :: RandomGen g => Rand g (Double, Double)
step2D = liftA2 (,) (getRandomR (-1,1)) (getRandomR (-1,1))

steps :: RandomGen g => Int -> Rand g [Double]
steps n = sequence (replicate n step)

path :: RandomGen g => Int -> Rand g [Double]
path n = liftA2 (scanl (+)) (step) (steps n)

steps2D :: RandomGen g => Int -> Rand g [(Double, Double)]
steps2D n = sequence (replicate n step2D)

main = do
    moves <- evalRandIO $ path 10
    print moves

