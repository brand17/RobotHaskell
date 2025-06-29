module Test where
import System.Exit (exitFailure)
import Numeric.LinearAlgebra
import Main
import Test.QuickCheck (quickCheck )

main = do
    let o' = matrix 3 [1 .. 15]
    print $ basisRowsMask o'
    putStrLn "This test always fails!"
    exitFailure