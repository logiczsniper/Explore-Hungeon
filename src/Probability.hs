module Probability
  ( getFloorType
  , getRandomInt
  , shiftProbability
  , nextProbability
  ) where

import           GameTypes
import           System.Random

-- Generates the next type of floor; either wet or dry.
getFloorType :: RandomList -> MapNumber -> MapType
getFloorType randomList mapNumber =
  let value = randomList !! mapNumber
   in if elem value [0 .. 68]
        then Dry
        else Wet

-- Get random integer using the coordinates as a 2 digit base-10 integer.
-- Using that value as an index, get the corresponding value in random list.
getRandomInt :: RandomList -> Coordinates -> Int
getRandomInt randomList coordinates@(x, y) = randomList !! (x * 10 + y)

-- Shift a probability by a given amount. If the cap is reached, must wrap around (back to 0).
shiftProbability :: Probability -> Int -> Probability
shiftProbability startProbability shift =
  map (incrementList shift) startProbability

incrementList :: Int -> Int -> Int
incrementList shift num
  | num == 99 = shift' - 1
  | (99 - num) < shift' = (num + shift') - 99
  | otherwise = num + shift'
  where
    shift' = adjustShift shift

-- Ensure the shift value does not get larger than the cap (of 99).
-- If this is the case, the probability shifting function will not work.
adjustShift :: Int -> Int
adjustShift start
  | start == 99 = 5
  | start > 99 = mod start 99
  | otherwise = start

-- Evaluates the next probability; given a value, is the value in the probability.
-- Applies the shift before making assertion.
nextProbability :: Int -> MapNumber -> Probability -> Bool
nextProbability value mapNumber probability = elem value newProbability
  where
    shift = mapNumber * 5
    newProbability = shiftProbability probability shift
