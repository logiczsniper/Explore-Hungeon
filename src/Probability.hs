module Probability
  ( floorsProbability
  , getRandomInt
  , shiftProbability
  ) where

import           GameTypes
import           System.Random

floorsProbability :: RandomList -> MapNumber -> MapType
floorsProbability randomList mapNumber =
  let value = randomList !! mapNumber
   in if elem value [0 .. 68]
        then Dry
        else Wet

getRandomInt :: RandomList -> Coordinates -> Int
getRandomInt randomList coordinates@(x, y) = randomList !! (x * 10 + y)

-- TODO: shifting not correct - later in game!
shiftProbability :: Probability -> Int -> Probability
shiftProbability startProbability shift =
  map (incrementList shift) startProbability

incrementList :: Int -> Int -> Int
incrementList shift num
  | num == 99 = shift - 1
  | (99 - num) < shift = (num + shift) - 99
  | otherwise = num + shift
