{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent
-- import NiceFork

import Control.Exception (evaluate)
-- import Control.Exception.Backtrace
import Control.Monad
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
import GHC.Stack (HasCallStack)
import Graphics.Gloss.Interface.IO.Animate hiding (Vector)
import Numeric.LinearAlgebra
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

readAccelerometers :: [Rod] -> (Vector R, Vector R)
readAccelerometers rod =
  let g = vector [0, -9.8]
      acc1 = scalar l1 * accDiff (head rod)
      acc2 = acc1 + scalar l2 * accDiff (last rod)
      accCM1 = g - scalar 0.5 * acc1
      accCM2 = g - scalar 0.5 * (acc1 + acc2)
      accLoc1 = rotMat (-(x $ head rod)) #> accCM1
      accLoc2 = rotMat (-(x $ last rod)) #> accCM2
   in (accLoc1, accLoc2)

sensors :: MVar [Rod] -> MVar Double -> Double -> Matrix R -> IO b
sensors refRod refDuty duty observations = do
  rod <- readMVar refRod
  let (accLoc1, accLoc2) = readAccelerometers rod
  -- print (accLoc1, accLoc2)
  _ <- takeMVar refDuty
  putMVar refDuty duty
  let gLoc = vector [-9.8, 0]
      obs = vjoin [scalar duty, accLoc1 - gLoc, accLoc2 - gLoc]
  -- print observations
  -- print obs
  -- print duty
  -- print ("duty", duty)
  let duty' = newDuty observations obs duty
  -- print ("duty'", duty')
  let o = fromRows [obs]
      o' = if rows observations == 0 then o else observations === o
      newObservations = basisCols o' $ basisRowsMask o'
  -- print (obs, newObservations)
  threadDelay 500000
  sensors refRod refDuty duty' newObservations

newDuty :: Matrix R -> Vector R -> R -> R
newDuty observations obs duty =
  let nextObs =
        (if rows observations == 0 then asRow obs else observations === asRow obs)
          ?? (Drop 1, Drop 1)
      matSize = size obs
      ratios =
        if rows nextObs == 0
          then (0 >< 0) [] :: Matrix R
          else
            let o' =
                  if rows observations == matSize
                    then observations
                    else
                      let o = fromRows [obs]
                          o'' = tr' $ if rows observations == 0 then o else observations === o
                       in tr' (basisCols o'' $ basisRowsMask o'') ?? (Drop 1, Drop 1)
             in fromMaybe (error "wrong observations") $
                  linearSolve o' nextObs
   in if rows ratios > 0
        then
          let a = head $ toRows ratios
              b =
                head $
                  toRows $
                    tr' $
                      ratios ?? (Drop 1, All)
                        * asColumn (subVector 1 (matSize - 1) obs)
           in -(dot a b / dot a a)
        else duty

basisCols :: (Element t, Eq a, Num a) => Matrix t -> [a] -> Matrix t
basisCols m mask =
  let inds = [i | (v, i) <- zip mask [0 ..], v /= 0]
   in m ?? (Pos (idxs inds), All)

basisRowsMask :: Matrix R -> [R]
basisRowsMask m
  | rows m == 0 = []
  | cols m == 1 =
      if abs m ! 0 ! 0 < 0.001
        then
          0 : basisRowsMask (dropRows 1 m)
        else [m ! 0 ! 0]
  | otherwise =
      let c1 = head (toRows m)
          i = maxIndex $ abs c1
          (m'', v) =
            if abs m ! 0 ! i > 0.001
              then
                let (r0, m') = (m ¿ [i], m ?? (All, Take i) ||| (m ?? (All, Drop $ i + 1)))
                 in (scalar (r0 ! 0 ! 0) * m' - r0 * takeRows 1 m', r0 ! 0 ! 0)
              else (dropRows 1 m, 0)
       in v : basisRowsMask (dropRows 1 m'')

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

main :: (HasCallStack) => IO ()
main = do
  -- setBacktraceMechanismState CostCentreBacktrace True
  -- let m = traceShowId $ head (toColumns $ matrix 1 [1 .. 15])
  -- print m
  -- print $ accDiff $ Var 489.8538573938557 315.9010928869936 (-394.2820661499558)
  -- a <- newMVar [Var 464.0897101 286.7705198 383.9401173, Var 64.1445146 17.1352783 153.2820900]
  -- sensors a -- [41126.130616413975,-198.31925023022438],[-46876.664903601355,-67505.96081714175]
  -- let observations =
  --       (2 >< 5)
  --         [ 0.0,
  --           0.0,
  --           -6.000571133729649e-16,
  --           4.89591802753484e-2,
  --           0.9783674831389163,
  --           0.0,
  --           7.632757290946039,
  --           -10.756081127611077,
  --           11.58863227766335,
  --           -5.9274815228834035
  --         ] ::
  --         Matrix R
  -- let obs = 5 |> [0.0, 6.968941764911328, -8.18639664049938, 12.155130063167785, -7.426331449571719]
  -- let a = newDuty observations obs 0
  -- let o' =
  --       (3 >< 3)
  --         [ 0.0,
  --           -6.000571133729649e-16,
  --           4.89591802753484e-2,
  --           7.632757290946039,
  --           -10.756081127611077,
  --           11.58863227766335,
  --           6.968941764911328,
  --           -8.18639664049938,
  --           12.155130063167785
  --         ] ::
  --         Matrix R
  -- let nextObs =
  --       (3 >< 5)
  --         [ 0.0,
  --           0.0,
  --           -6.000571133729649e-16,
  --           4.89591802753484e-2,
  --           0.9783674831389163,
  --           0.0,
  --           7.632757290946039,
  --           -10.756081127611077,
  --           11.58863227766335,
  --           -5.9274815228834035,
  --           0.0,
  --           6.968941764911328,
  --           -8.18639664049938,
  --           12.155130063167785,
  --           -7.426331449571719
  --         ]
  -- let a = linearSolve o' nextObs
  -- print a
  -- getLine
  refRods <- newMVar initialState
  refDuty <- newMVar 0
  _ <- forkIO $ do
    animateIO FullScreen black (\_ -> picture <$> readMVar refRods) (control refRods refDuty)
  sensors refRods refDuty 0 $ fromLists []

initialState :: [Rod]
initialState =
  [ Rod (pi / 2) 0 0,
    Rod ((pi / 2) + 0.1) 0 0
  ]

controlCycle :: MVar [Rod] -> MVar Double -> Controller -> TimeSpec -> IO ()
controlCycle refRods refDuty c t0 = do
  t <- getTime Monotonic
  duty <- readMVar refDuty
  rods <- takeMVar refRods
  updatedRods <- evaluate (updateRods rods duty (t - t0))
  -- putStrLn "state updated"
  putMVar refRods updatedRods
  controllerSetRedraw c
  threadDelay 1
  controlCycle refRods refDuty c t

control :: MVar [Rod] -> MVar Double -> Controller -> IO ()
control refRods refDuty c = do
  t <- getTime Monotonic
  void . forkIO $ controlCycle refRods refDuty c t

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
