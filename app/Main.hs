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

data Vars = Vars {time :: UTCTime, pos :: Float, speed :: Float, acc :: Float}

model :: Vars -> IO ()
model v = do
  t <- getCurrentTime
  putStrLn "Model"
  let a = fromBlocks [[ident 5, 7, row [10, 20]], [3, diagl [1, 2, 3], 0]]
  threadDelay 1000000
  model v

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
  let v = Vars t 0 0 0
  model v

-- return ()