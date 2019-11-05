module PointerFunctions
  ( translatePointer
  , movePointer
  , tilePointerInteraction
  ) where

import           Data.List
import           GameTypes
import           GenerateMap
import           Graphics.Gloss
import           Numeric.Extra

{- The number starts at 0. This gets the nth term in sequence- non zero. -}
translatePointer :: Pointer -> Coordinates -> Pointer
translatePointer pointer (x, y) =
  translate
    (intToFloat $ 16 * (x + 1) - 408)
    (intToFloat $ 16 * (y + 1) - 408)
    pointer

movePointer :: GameState -> Coordinates -> GameState
movePointer startState newCoords =
  GameState
    { tiles = tiles startState
    , effects = effects startState
    , pointerCoords = newCoords
    , mapNumber = mapNumber startState
    }

tilePointerInteraction ::
     Coordinates -> GameState -> PictureList -> RandomList -> GameState
tilePointerInteraction coords@(x, y) startState images randomList =
  if elem coords $ getEntranceCoords $ tiles startState
    then let newMapNumber = mapNumber startState + 1
             addition = calculateAddition newMapNumber
             newTiles =
               generateMap images (map (+ addition) randomList) newMapNumber
          in GameState
               { tiles = newTiles
               , effects = []
               , pointerCoords = (x, y)
               , mapNumber = newMapNumber
               }
    else startState

calculateAddition :: MapNumber -> Int
calculateAddition input =
  if mod input 2 == 0
    then input + 1
    else input + 3

getEntranceCoords :: TileList -> [Coordinates]
getEntranceCoords tiles =
  [ (columnNumber tile + 12, rowNumber tile + 12)
  | tile <- tiles
  , isEntrance tile
  ]
