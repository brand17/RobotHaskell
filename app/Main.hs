{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent
-- import NiceFork

import Control.Exception (evaluate)
import Control.Monad
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
import Graphics.Gloss.Interface.IO.Animate hiding (Vector)
import Numeric.LinearAlgebra (cross, linearSolve, (#>))
import Numeric.LinearAlgebra.Data
import System.Clock
import Text.Printf

accDiff :: Rod -> Vector R
accDiff r =
  let cx = cos $ x r
      cy = sin $ x r
      v =
        vector
          [ -(cy * x'' r) - cx * x' r ** 2,
            cx * x'' r - cy * x' r ** 2
          ]
   in v

rotMat :: R -> Matrix R
rotMat a =
  let c = cos a
      s = sin a
   in fromLists [[c, -s], [s, c]] :: Matrix R

type Sensors = [Vector R]

sensors :: MVar [Rod] -> MVar Sensors -> IO b
sensors refRod refSensors = do
  rod <- readMVar refRod
  let g = vector [0, -9.8]
      acc1 = scalar l1 * accDiff (head rod)
      acc2 = acc1 + scalar l2 * accDiff (last rod)
      accCM1 = g - scalar 0.5 * acc1
      accCM2 = g - scalar 0.5 * (acc1 + acc2)
      accLoc1 = rotMat (-(x $ head rod)) #> accCM1
      accLoc2 = rotMat (-(x $ last rod)) #> accCM2
  print (accLoc1, accLoc2)
  threadDelay 5000000
  sensors refRod refSensors

data Rod = Rod {x :: Double, x' :: Double, x'' :: Double}

newVar :: Rod -> TimeSpec -> Rod
newVar v t =
  let dt = realToFrac t / 1000000000
   in v {x = x v + (x' v + 0.5 * x'' v * dt) * dt, x' = x' v + x'' v * dt}

instance Show Rod where
  show :: Rod -> String
  show v = printf "%.7f,%.7f,%.7f" (x v) (x' v) (x'' v)

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

updateRods :: [Rod] -> Double -> TimeSpec -> [Rod]
updateRods rods duty t =
  let v_new = [newVar a t | a <- rods]
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
                  [0, ident 3, konst 0 (3, 3)]
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
            vector [0, 0, duty],
            0
          ]
      b = head $ toColumns $ fromMaybe (error "wrong matrix") (linearSolve m $ fromColumns [a])
      alpha1 = b `atIndex` 9
      alpha2 = b `atIndex` 12 + alpha1
   in [v {x'' = a} | (v, a) <- zip v_new [alpha1, alpha2]]

main :: IO ()
main = do
  -- let m = traceShowId $ head (toColumns $ matrix 1 [1 .. 15])
  -- print m
  -- print $ accDiff $ Var 489.8538573938557 315.9010928869936 (-394.2820661499558)
  -- a <- newMVar [Var 464.0897101 286.7705198 383.9401173, Var 64.1445146 17.1352783 153.2820900]
  -- sensors a -- [41126.130616413975,-198.31925023022438],[-46876.664903601355,-67505.96081714175]
  refRods <- newMVar initialState
  refSensors <- newMVar [konst 0 2, konst 0 2]
  _ <- forkIO $ do
    sensors refRods refSensors
  animateIO FullScreen black (\_ -> picture <$> readMVar refRods) (control refRods refSensors)

initialState :: [Rod]
initialState =
  [ Rod (pi / 2) 0 0,
    Rod ((pi / 2) + 0.1) 0 0
  ]

controlCycle :: MVar [Rod] -> MVar Sensors -> Double -> Controller -> TimeSpec -> IO ()
controlCycle refRods refSensors duty c t0 = do
  rods <- takeMVar refRods
  t <- getTime Monotonic
  s <- readMVar refSensors
  updatedRods <- evaluate (updateRods rods duty (t - t0))
  -- putStrLn "state updated"
  putMVar refRods updatedRods
  controllerSetRedraw c
  threadDelay 1
  controlCycle refRods refSensors duty c t

control :: MVar [Rod] -> MVar Sensors -> Controller -> IO ()
control r e c = do
  t <- getTime Monotonic
  void . forkIO $ controlCycle r e 0 c t

picture :: [Rod] -> Picture
picture r = do
  let [a1, a2] = fmap (realToFrac . x) r
  Scale 100 100 $
    Rotate (180 / pi * a1 - 90) $
      Color green $
        Line
          [ (0, 0),
            (0, realToFrac l1),
            ( realToFrac l2 * sin (a2 - a1),
              realToFrac l1 + realToFrac l2 * cos (a2 - a1)
            )
          ]
