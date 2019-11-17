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

-- Replicates current game state, except using the given coords.
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

-- Modifies the game state if the pointer was on an entrance.
tilePointerInteraction ::
     Coordinates -> GameState -> PictureList -> RandomList -> GameState
tilePointerInteraction pointerCoords@(x, y) startState images randomList =
  let entrances = getEntranceCoords $ tiles startState
      newMapNumber = mapNumber startState + 1
      newTiles = generateMap images randomList newMapNumber
      (width, length) = getDimensions newTiles
      newPointerState =
        PointerState
          { frames = frames $ pointerState startState
          , index = index $ pointerState startState
          , size = 1.7
          , coords = (width + 3, length + 3)
          }
   in if entrances == [] || elem pointerCoords entrances
        then GameState
               { tiles = newTiles
               , pointerState = newPointerState
               , mapNumber = newMapNumber
               , currentDuration = 0.0
               }
        else startState

-- Get a list of coords for all tiles classified as an entrance.
getEntranceCoords :: TileList -> [Coordinates]
getEntranceCoords tiles =
  [ (columnNumber tile + 12, rowNumber tile + 12)
  | tile <- tiles
  , isEntrance tile
  ]

-- Get the length and width of the game using the tiles.
getDimensions :: TileList -> Dimensions
getDimensions tiles =
  let maxX = maximum [columnNumber tile | tile <- tiles]
      maxY = maximum [rowNumber tile | tile <- tiles]
   in (maxX, maxY)
