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

engineOn = True

sensors :: MVar [Rod] -> MVar Double -> Double -> Matrix R -> IO b
sensors refRod refDuty duty observations = do
  rod <- readMVar refRod
  let readAccelerometers :: [Rod] -> (Vector R, Vector R)
      -- readAccelerometers rod | trace ("readAccelerometers " ++ show rod) False = undefined
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
      o' = if rows observations == 0 then o else basisRows $ observations === o
  -- o'' = if rows o' < 6 then o' else takeLastRows 5 o'
  -- newObservations = basisCols o'
  -- print (obs, newObservations)
  threadDelay 5000
  -- t <- getTime Monotonic
  -- print t
  sensors refRod refDuty duty' o'

basisRowsInds :: Matrix R -> ([Int], [Int]) -> ([Int], [Int])
-- basisRowsInds m (c, c') | trace ("basisRowsInds " ++ show m ++ " " ++ show (c, c')) False = undefined
basisRowsInds m (c, c')
  | rows m == 0 || cols m == 0 = (c, c')
  | otherwise =
      let lastRow = rows m - 1
          csm = m ?? (All, Pos $ sortIndex (negate $ abs (m ! lastRow)))
       in if abs (csm ! lastRow ! 0) == 0
            then basisRowsInds (takeRows lastRow csm) (c, init c')
            else
              if rows csm == 1
                then (last c' : c, init c')
                else
                  let m'' =
                        let (r, m') = (dropRows lastRow csm, (takeRows lastRow csm))
                         in scalar (r ! 0 ! 0) * m' - r * takeColumns 1 m'
                   in basisRowsInds (dropColumns 1 m'') (last c' : c, init c')

basisRows :: Matrix R -> Matrix R
basisRows m =
  let i = fst $ basisRowsInds m ([], [0 .. rows m - 1])
   in m ?? (Pos (idxs i), All)

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
           in boundAbs (-(dot a b / dot a a)) 100
        else duty

basisColsInds :: Matrix R -> ([Int], [Int]) -> ([Int], [Int])
-- basisColsInds m (c, c') | trace ("basisColsInds " ++ show m ++ " " ++ show (c, c')) False = undefined
basisColsInds m (c, c')
  | rows m == 0 || cols m == 0 = (c, c')
  | otherwise =
      let rsm = m ?? (Pos $ sortIndex (negate $ abs (head $ toColumns m)), All)
       in if abs rsm ! 0 ! 0 == 0
            then basisColsInds (dropColumns 1 rsm) (c, tail c')
            else
              if cols rsm == 1
                then (c ++ [head c'], tail c')
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

bound x mn mx = max mn (min mx x)

boundAbs x b = bound x (-b) b

updateRods :: [Rod] -> Double -> TimeSpec -> [Rod]
updateRods rods duty t =
  let newVar :: Rod -> TimeSpec -> Rod
      newVar v t =
        let dt = realToFrac t / 1000000000
         in v {x = x v + (x' v + 0.5 * x'' v * dt) * dt, x' = x' v + x'' v * dt}

      newVars :: [Rod] -> TimeSpec -> [Rod]
      newVars r t =
        let dt = realToFrac t / 1000000000
            potentEnergy :: [Rod] -> Double
            potentEnergy r = 9.8 * (m1 * (l1 * sin (x (head r)) + l1) + m2 * (l2 * sin (x (last r)) + l2))
            kinetEnergy :: [Rod] -> Double
            kinetEnergy rods =
              let x0 = x (head rods)
                  x1 = x (last rods)
                  cm2 =
                    scalar l1 * vector [cos x0, sin x0]
                      + scalar l2 / 2 * vector [cos x1, sin x1]
                  v1 = vector [-cm2 ! 1, cm2 ! 0] * 2 * pi * scalar (x' (head rods))
                  v2 = scalar (pi * l2 * x' (last rods)) * vector [-sin x1, cos x1]
                  vcm = v1 + v2
                  keRodCenter :: R -> R -> R -> R
                  keRodCenter m l w = 1 / 24 * m * l * l * w * w
                  keRodEnd :: R -> R -> R -> R
                  keRodEnd m l w = 1 / 6 * m * l * l * w * w
               in keRodEnd m1 l1 (x' (head rods))
                    + m2 * 0.5 * dot vcm vcm
                    + keRodCenter m2 l2 (x' (last rods))
            r' =
              [ v {x = x v + (x' v + 0.5 * x'' v * dt) * dt, x' = x' v + x'' v * dt}
              | v <- r
              ]
            pe = potentEnergy r
            ke = kinetEnergy r
            pe' = potentEnergy r'
            ke' = kinetEnergy r'
            k =
              -- trace (show pe ++ " " ++ show ke ++ " " ++ show pe' ++ " " ++ show ke') $
              if ke' == 0
                then 1
                else
                  (pe + ke - pe') / ke'
            r'' =
              [ v' {x' = x' v' * (k ** 0.5)}
              | v' <- r'
              ]
         in r''

      -- v_new = [newVar a t | a <- rods]
      v_new = newVars rods t
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
            vector [0, 0, if engineOn then duty else 0],
            0
          ]
      b = head $ toColumns $ fromMaybe (error "wrong matrix") (linearSolve m $ fromColumns [a])
      alpha1 = b `atIndex` 9
      alpha2 = b `atIndex` 12 + alpha1
   in [v {x'' = a} | (v, a) <- zip v_new [alpha1, alpha2]]

main :: (HasCallStack) => IO ()
main = do
  setBacktraceMechanismState CostCentreBacktrace True
  --     obs = vector [-0.6554664844616452, 5.871868922584511e-3, 4.158951088345384, 1.4737820668779023, -4.3550224506076605e-4]
  --     d = newDuty m obs (-0.6554664844616452)
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
