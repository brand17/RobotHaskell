{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent
-- import NiceFork

import Control.Exception (evaluate)
import Control.Exception.Backtrace
import Control.Monad
import Data.Maybe (fromMaybe)
import Debug.Trace (trace, traceShowId)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import GHC.Stack (HasCallStack)
import Graphics.Gloss.Interface.IO.Animate hiding (Vector)
import Internal.Element (takeLastRows)
import Numeric.LinearAlgebra
import System.Clock
import System.Exit
import Text.Printf
import Prelude hiding ((<>))

sensors :: MVar [Rod] -> MVar Double -> Double -> Matrix R -> IO b
sensors refRod refDuty duty observations = do
  rod <- readMVar refRod
  let readAccelerometers :: [Rod] -> (Vector R, Vector R)
      readAccelerometers rod =
        let g = vector [0, -9.8]
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

            acc1 = scalar l1 * accDiff (head rod)
            acc2 = acc1 + scalar l2 * accDiff (last rod)
            accCM1 = g - scalar 0.5 * acc1
            accCM2 = g - scalar 0.5 * (acc1 + acc2)
            accLoc1 = rotMat (-(x $ head rod)) #> accCM1
            accLoc2 = rotMat (-(x $ last rod)) #> accCM2
         in (accLoc1, accLoc2)

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
      o'' = if rows o' < 6 then o' else takeLastRows 5 o'
  -- newObservations = basisCols o'
  -- print (obs, newObservations)
  threadDelay 500000
  sensors refRod refDuty duty' o''

newDuty :: Matrix R -> Vector R -> R -> R
-- newDuty observations obs duty | trace ("newDuty " ++ show observations ++ " " ++ show obs ++ " " ++ show duty) False = undefined
newDuty observations obs duty =
  let o = fromRows [obs]
      o' = if rows observations == 0 then o else observations === o
      nextObs = o' ?? (Drop 1, Drop 1)
      (ratios, inds) =
        -- traceShowId $
        if rows nextObs == 0
          then ((0 >< 0) [] :: Matrix R, [])
          else
            let (o'', i') =
                  if rows observations /= cols observations
                    then
                      let i = fst $ basisColsInds observations ([], [0 .. 4])
                       in (observations ?? (All, Pos (idxs i)), i)
                    else (observations, [0 .. 4])
             in ( fromMaybe (error "wrong observations") $
                    linearSolve o'' nextObs,
                  i'
                )
   in if rows ratios > 1 && head inds == 0
        then
          let a = ratios ! 0
              r' = dropRows 1 ratios
              o' = fromRows [obs] ¿ tail inds
              b = (o' <> r') ! 0
           in max (-1000) (min 1000 (-(dot a b / dot a a)))
        else duty

basisColsInds :: Matrix R -> ([Int], [Int]) -> ([Int], [Int])
-- basisColsInds m (c, c') | trace ("basisColsInds " ++ show m ++ " " ++ show (c, c')) False = undefined
basisColsInds m (c, c')
  | rows m == 0 || cols m == 0 = (c, c')
  | otherwise =
      let rsm = m ?? (Pos $ sortIndex (scalar (-1) * abs (head $ toColumns m)), All)
       in if abs rsm ! 0 ! 0 < 0.001
            then basisColsInds (dropColumns 1 rsm) (c, tail c')
            else
              if cols rsm == 1
                then (c, c')
                else
                  let m'' =
                        let (c0, m') = (takeColumns 1 rsm, (dropColumns 1 rsm))
                         in scalar (c0 ! 0 ! 0) * m' - c0 * takeRows 1 m'
                   in basisColsInds (dropRows 1 m'') (c ++ [head c'], tail c')

basisCols :: Matrix R -> Matrix R
basisCols m =
  let i = fst $ basisColsInds m ([], [0 .. 4])
   in m ?? (All, Pos (idxs i))

data Rod = Rod {x :: Double, x' :: Double, x'' :: Double}

instance Show Rod where
  show :: Rod -> String
  show v = printf "%.7f,%.7f,%.7f" (x v) (x' v) (x'' v)

l1 :: R
l1 = 1

l2 :: R
l2 = 2 * sqrt 2

updateRods :: [Rod] -> Double -> TimeSpec -> [Rod]
updateRods rods duty t =
  let newVar :: Rod -> TimeSpec -> Rod
      newVar v t =
        let dt = realToFrac t / 1000000000
         in v {x = x v + (x' v + 0.5 * x'' v * dt) * dt, x' = x' v + x'' v * dt}

      v_new = [newVar a t | a <- rods]
      cpMatrix v = fromLists [[0, -(v ! 2), v ! 1], [v ! 2, 0, -(v ! 0)], [-(v ! 1), v ! 0, 0]] :: Matrix R
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
  setBacktraceMechanismState CostCentreBacktrace True
  --     obs = vector [-1.226476661216117, 49.35569019353245, 0.6330242214091544, -31.078145446208485, -46.393167375108305]
  --     d = newDuty observations obs (-1.226476661216117)
  -- print d
  -- exitSuccess
  refRods <- newMVar initialState
  refDuty <- newMVar 1
  _ <- forkIO $ do
    animateIO FullScreen black (\_ -> picture <$> readMVar refRods) (control refRods refDuty)
  sensors refRods refDuty 1 $ fromLists []

initialState :: [Rod]
initialState =
  [ Rod (pi / 2) 0 0,
    Rod ((pi / 2) + 0.1) 0 0
  ]

control :: MVar [Rod] -> MVar Double -> Controller -> IO ()
control refRods refDuty c = do
  t <- getTime Monotonic
  let controlCycle :: MVar [Rod] -> MVar Double -> Controller -> TimeSpec -> IO ()
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

  void . forkIO $ controlCycle refRods refDuty c t

picture :: [Rod] -> Picture
-- picture r | trace ("picture " ++ show r) False = undefined
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
