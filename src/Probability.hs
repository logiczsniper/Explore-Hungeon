module Probability
  ( floorsProbability
  ) where

import           GameTypes
import           System.Random

floorsProbability :: StdGen -> FloorType
floorsProbability generator =
  let tst = nextProbability generator 10
   in if elem tst [1 .. 8]
        then Dry
        else Wet

nextProbability :: StdGen -> Int -> Int
nextProbability generator bound = fst $ randomR (1, bound) generator
