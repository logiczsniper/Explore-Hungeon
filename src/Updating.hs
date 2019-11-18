module Updating
  ( update
  ) where

import           GameTypes
import           Graphics.Gloss

-- Update the game state.
update :: Float -> GameState -> GameState
update time initState =
  GameState
    { tiles = tiles initState
    , pointerState =
        PointerState
          { frames = frames $ pointerState initState
          , index = incrementIndex increment (index $ pointerState initState)
          , coords = coords $ pointerState initState
          , size =
              updateSize (size $ pointerState initState) $ mapNumber initState
          }
    , mapNumber = mapNumber initState
    , currentDuration = (currentDuration initState) + time
    }
  where
    increment = round $ time / (1 / 15)

-- Increase the index of the poiner frame in order to get the next
-- frame in the animation.
incrementIndex :: Int -> Int -> Int
incrementIndex x 0 = x
incrementIndex startIndex increment
  | startIndex == 5 = incrementIndex 0 (increment - 1)
  | startIndex < 5 = incrementIndex (startIndex + 1) (increment - 1)

-- Decrease the size of the pointer. Varies with mapNumber.
updateSize :: Size -> MapNumber -> Size
updateSize x mapNumber
  | mapNumber == 0 = x
  | x <= 0 = 0
  | otherwise = x - (0.001 + (fromIntegral mapNumber) * 0.0012)
