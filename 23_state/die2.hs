module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample



intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix

  -- Use error sparingly (Maybe / Either usually preferred)
  x ->
    error $
      "intToDie got non 1-6 integer: "
      ++ show x

-- verbose:
-- rollDie :: State StdGen Die
-- rollDie = state $ do
--   (n, s) <- randomR (1, 6)
--   return (intToDie n, s)

-- shortened:
rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6)) 
  -- state :: Monad m => (s -> (a, s)) -> StateT s m a (construct state monad from fn)

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie' rollDie' rollDie'

-- This repeats a single die value:
-- repeat :: a -> [a]
-- infiniteDie :: State StdGen [Die]
-- infiniteDie = repeat <$> rollDie

-- This repeats the state action which produces a die:
-- replicateM :: Monad m => Int -> m a -> m [a]
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =

        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
              (count + 1) nextGen

-- alternatively, randomIO uses IO to get fresh value each time 
-- w/o needing to create unique value for StdGen:
-- randomIO :: Random a => IO a
-- rs = (rollsToGetTwenty . mkStdGen)

-- rs <$> randomIO 
-- 6

-- roll your own 
-- 1)
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =

        let (die, nextGen) =
              randomR (1, 6) gen
        in go (sum + die)
              (count + 1) nextGen

-- 2)
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where
      go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
      go sum count dice gen
        | sum >= n = (count, dice)
        | otherwise =

          let (die, nextGen) =
                randomR (1, 6) gen
          in go (sum + die)
                (count + 1) (intToDie die : dice) nextGen 