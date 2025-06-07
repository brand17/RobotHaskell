{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent
-- import NiceFork
import Control.Monad.State
import Data.Time.Clock
import Numeric.LinearAlgebra.Data

-- import System.Random

-- type RandomState a = State StdGen a

-- getRandom :: (Random a) => RandomState a
-- getRandom =
--   get >>= \gen ->
--     let (val, gen') = random gen
--      in put gen'
--           >> return val

-- getTwoRandoms :: (Random a) => RandomState (a, a)
-- getTwoRandoms = liftA2 (,) getRandom getRandom

-- type Stack = [Int]

-- pop :: State Stack Int
-- pop = state $ \(x : xs) -> (x, xs)

-- push :: Int -> State Stack ()
-- push a = state $ \xs -> ((), a : xs)

-- stackManip :: State Stack Int
-- stackManip = do
--   push 3
--   _ <- pop
--   pop

-- b :: (Int, Stack)
-- b = runState stackManip [5, 8, 2, 1]

sensors :: IO b
sensors = do
  putStrLn "Sensors"
  threadDelay 5000000
  sensors

data Var = Var {x :: Double, x' :: Double, x'' :: Double}

newVar :: Var -> NominalDiffTime -> Var
newVar v t =
  let dt = realToFrac t
   in Var (x v + x' v + 0.5 * x'' v * dt * dt) (x' v + x'' v * dt) (x'' v)

r1 :: Vector R
r1 = vector [0, 0, 0]

l1 :: Double
l1 = 1

l2 :: Double
l2 = 2 * sqrt 2

z2 :: Vector R
z2 = vector [0, 0, 1]

m1 :: Double
m1 = 3

m2 :: Double
m2 = 4

i1 :: Matrix Double
i1 = scalar (m1 * l1 ** 2 / 12) * ident 3

i2 :: Matrix Double
i2 = scalar (m2 * l2 ** 2 / 12) * ident 3

_W1 :: Vector R
_W1 = vector [0, 9.8 * m1, 1]

_W2 :: Vector R
_W2 = vector [0, 9.8 * m2, 1]

tExt :: Vector R
tExt = vector [0, 0, 0]

_Q2 :: Double
_Q2 = 0

updateVars :: [Var] -> NominalDiffTime -> [Var]
updateVars v t =
  let v1 = [newVar a t | a <- v]
      c1 = r1 + scalar l1 * vector [sin $ x $ head v1, cos $ x $ head v1, 0]
      m = fromBlocks [[ident 5, 7, row [10, 20]], [3, diagl [1, 2, 3], 0]]
   in []

model :: [Var] -> UTCTime -> IO ()
model v t0 = do
  putStrLn "Model"
  t <- getCurrentTime
  let v1 = updateVars v $ diffUTCTime t t0
  threadDelay 1000000
  model v1 t

main :: IO ()
main = do
  putStrLn "press Enter to exit the program"
  threadId <- forkIO $ do
    sensors
  -- putStrLn "Something"
  -- threadDelay 5000000 -- wait 5 seconds
  -- putStrLn "Something Else"
  -- _ <- getLine
  -- main
  t <- getCurrentTime
  let v = [Var 0 0 0, Var 0 0 0]
  model v t

-- return ()