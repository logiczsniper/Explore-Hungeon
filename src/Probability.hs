module Probability
  ( floorsProbability
  , nextProbability
  ) where

import           GameTypes
import           System.Random

floorsProbability :: RandomList -> MapNumber -> MapType
floorsProbability randomList mapNumber =
  let value = randomList !! mapNumber
   in if elem value [0 .. 68]
        then Dry
        else Wet

nextProbability :: RandomList -> Coordinates -> Int
nextProbability randomList coordinates@(x, y) = randomList !! (x * 10 + y)
