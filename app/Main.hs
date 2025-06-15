{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent
-- import NiceFork
-- import Control.Monad.State

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Debug.Trace (traceShowId)
import Numeric.LinearAlgebra (cross, linearSolve, (#>))
import Numeric.LinearAlgebra.Data
import Text.Printf

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
   in Var (x v + (x' v + 0.5 * x'' v * dt) * dt) (x' v + x'' v * dt) (x'' v)

updateAcc :: Var -> Double -> Var
updateAcc v = Var (x v) (x' v)

instance Show Var where
  show :: Var -> String
  show v = printf "%.7f,%.7f,%.7f" (x v) (x' v) (x'' v)

-- r1 :: Vector R
-- r1 = vector [0, 0, 0]

l1 :: R
l1 = 1

l2 :: R
l2 = 2 * sqrt 2

z2 :: Vector R
z2 = vector [0, 0, 1]

m1 :: R
m1 = 3

m2 :: R
m2 = 4

i1 :: Matrix R
i1 = scalar (m1 * l1 ** 2 / 12) * diagl [1, 1, 0]

i2 :: Matrix R
i2 = scalar (m2 * l2 ** 2 / 12) * diagl [1, 1, 0]

_W1 :: Vector R
_W1 = vector [0, -(9.8 * m1), 0]

_W2 :: Vector R
_W2 = vector [0, -(9.8 * m2), 0]

tExt :: Vector R
tExt = vector [0, 0, 0]

_Q2 :: R
_Q2 = 0

cpMatrix :: Vector R -> Matrix R
cpMatrix v = fromLists [[0, -(v ! 2), v ! 1], [v ! 2, 0, -(v ! 0)], [-(v ! 1), v ! 0, 0]] :: Matrix R

updateVars :: [Var] -> NominalDiffTime -> [Var]
updateVars v t =
  let v_new = [newVar a t | a <- v]
      r2 = scalar l1 * vector [cos $ x $ head v_new, sin $ x $ head v_new, 0]
      c1 = r2 / 2
      c2 = scalar l2 * vector [cos $ x $ last v_new, sin $ x $ last v_new, 0] / 2
      w1 = vector [0, 0, x' $ head v_new]
      w2 = vector [0, 0, x' $ last v_new]
      v2 = scalar (l1 * x' (head v_new)) * vector [-(sin $ x $ head v_new), cos $ x $ head v_new, 0]
      q2' = x' (last v_new) - x' (head v_new)
      m1mat = scalar m1 * ident 3
      m2mat = scalar m2 * ident 3
      m =
        fromBlocks
          [ [ asColumn $ vjoin [cross (-r2) z2, -z2],
              fromBlocks
                [ [konst 0 (3, 3), ident 3, cpMatrix r2, cpMatrix r2],
                  [0, 0, 0, ident 3]
                ],
              0
            ],
            [ 0,
              fromBlocks
                [ [m1mat, 0, cpMatrix $ scalar (-m1) * c1, 0],
                  [m2mat, m2mat, cpMatrix $ scalar (-m2) * c2, cpMatrix $ scalar (-m2) * c2],
                  [0, 0, i1, 0],
                  [0, 0, i2, i2]
                ],
              fromBlocks
                [ [-ident 3, 0, 0, 0],
                  [0, -ident 3, 0, 0],
                  [cpMatrix c1, cpMatrix r2, -ident 3, 0],
                  [0, cpMatrix c2, 0, -ident 3]
                ]
            ],
            [ 0,
              fromBlocks
                [ [ident 3, konst 0 (3, 9)],
                  [konst 0 (3, 3), 0]
                ],
              fromBlocks
                [ [konst 0 (3, 6), 0, 0],
                  [0, ident 3, ident 3]
                ]
            ],
            [ 0,
              0,
              fromBlocks
                [ [konst 0 (1, 9), asRow z2]
                ]
            ]
          ]
      a =
        vjoin
          [ cross r2 $ cross w2 $ cross (w2 - w1) v2,
            cross w2 z2 * scalar q2',
            _W1 - cross w1 (scalar m1 * cross w1 c1),
            _W2 - cross w2 (scalar m2 * cross w2 c2),
            -(w1 `cross` (i1 #> w1)),
            -(w2 `cross` (i2 #> w2)),
            konst 0 3,
            konst 0 3,
            0
          ]
      b = head $ toColumns $ fromMaybe (error "wrong matrix") (linearSolve m $ fromColumns [a])
      alpha1 = b `atIndex` 9
      alpha2 = b `atIndex` 12 + alpha1
   in [updateAcc v a | (v, a) <- zip v_new [alpha1, alpha2]]

model :: [Var] -> UTCTime -> IO ()
model v t0 = do
  -- putStrLn "Model"
  t <- getCurrentTime
  let tt = realToFrac $ diffUTCTime t t0
  let v1 = updateVars v $ diffUTCTime t t0
  printf "%s,%.7f\n" (show v1) (tt :: Double)
  threadDelay 1
  model v1 t

main :: IO ()
main = do
  -- let m = fromBlocks [[ident 2, 1], [matrix 1 [1 .. 3], matrix 4 [1 .. 12]]] :: Matrix R
  -- let m = traceShowId $ head (toColumns $ matrix 1 [1 .. 15])
  -- print m

  threadId <- forkIO $ do
    sensors
  t <- getCurrentTime
  let v = [Var (pi / 2 :: R) 0 0, Var ((pi / 2 :: R) + 0.1) 0 0]
  model v t

-- return ()