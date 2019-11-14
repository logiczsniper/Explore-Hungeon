module Probability
  ( floorsProbability
  , getRandomInt
  , shiftProbability
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

getRandomInt :: RandomList -> Coordinates -> Int
getRandomInt randomList coordinates@(x, y) = randomList !! (x * 10 + y)

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

adjustShift :: Int -> Int
adjustShift start
  | start == 99 = 5
  | start > 99 = adjustShift $ start - 99
  | otherwise = start

nextProbability :: Int -> MapNumber -> Probability -> Bool
nextProbability value mapNumber probability = elem value newProbability
  where
    shift = mapNumber * 5
    newProbability = shiftProbability probability shift
