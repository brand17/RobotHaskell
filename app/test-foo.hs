module Test where

import Main
import Numeric.LinearAlgebra
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck (quickCheck)

main = do
  -- quickCheck ((basisColsMask $ fromLists [[2, 0], [3, 0], [0, 4]]) == [2, 0, 8])
  -- quickCheck (basisColsInds (fromLists [[1, 0, 0], [0, 1, 0]]) ([], [0 .. 2]) == ([0, 1], [2]))
  -- quickCheck (basisColsInds (fromLists [[1, 1, 0], [0, 0, 1]]) ([], [0 .. 2]) == ([0, 2], [1]))
  -- quickCheck (basisColsInds (fromLists [[1, 1, 1], [0, 0, 0]]) ([], [0 .. 2]) == ([0], [1, 2]))
  -- quickCheck (basisColsInds (fromLists [[0, 0, 0], [0, 0, 0]]) ([], [0 .. 2]) == ([], [0, 1, 2]))
  -- quickCheck (basisColsInds (fromLists [[0, 1, 0], [0, 0, 0]]) ([], [0 .. 2]) == ([1], [0, 2]))
  -- quickCheck (basisColsInds (fromLists [[1, 2, 3], [2, 4, 6]]) ([], [0 .. 2]) == ([2], [0, 1]))

  exitSuccess