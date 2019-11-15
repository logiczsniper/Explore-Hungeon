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
    , pointerState =
        PointerState
          { frames = frames $ pointerState startState
          , index = index $ pointerState startState
          , size = size $ pointerState startState
          , coords = newCoords
          }
    , mapNumber = mapNumber startState
    , currentDuration = currentDuration startState
    }

tilePointerInteraction ::
     Coordinates -> GameState -> PictureList -> RandomList -> GameState
tilePointerInteraction pointerCoords@(x, y) startState images randomList =
  let entrances = getEntranceCoords $ tiles startState
      newMapNumber = mapNumber startState + 1
      newTiles = generateMap images randomList newMapNumber
      newPointerState =
        PointerState
          { frames = frames $ pointerState startState
          , index = index $ pointerState startState
          , size = 1.7
          , coords = coords $ pointerState startState
          }
   in if entrances == [] || elem pointerCoords entrances
        then GameState
               { tiles = newTiles
               , pointerState = newPointerState
               , mapNumber = newMapNumber
               , currentDuration = 0.0
               }
        else startState

getEntranceCoords :: TileList -> [Coordinates]
getEntranceCoords tiles =
  [ (columnNumber tile + 12, rowNumber tile + 12)
  | tile <- tiles
  , isEntrance tile
  ]
