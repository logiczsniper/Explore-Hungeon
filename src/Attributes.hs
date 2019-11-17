module Attributes
  ( window
  , backgroundColour
  , fps
  , initialState
  ) where

import           GameTypes
import           GenerateMap
import           Graphics.Gloss

{-
  Column and row setup:
  ---------------------
  |           C24R24 |
  |                  |
  |         X        |
  |C0R1 C1R1         |
  |C0R0 C1R0         |
  ---------------------

This is effectively an 800 by 800 grid in terms of
translation. The bottom left corner would be a translation (-400) (-400),
and a top right translation 400 400.

The translation is calculated via sequence
  Tn = 16n - 416
where n = row or column number + 1
-}
-- Some defining helper main functions. Note: 50 * 50 tiles at 16px each max.
window :: Display
window = InWindow "Explore" (width, height) (offset, offset)

backgroundColour :: Color
backgroundColour = makeColorI 19 19 19 255

-- Initialize the game with this game state.
initialState :: PictureList -> PictureList -> RandomList -> GameState
initialState images pointerFrames randomList =
  let startingMapNumber = 0
      startingTiles = generateMap images randomList startingMapNumber
      startingPointerState =
        PointerState
          {frames = pointerFrames, index = 0, coords = (20, 20), size = 1.7}
   in GameState
        { tiles = startingTiles
        , pointerState = startingPointerState
        , mapNumber = startingMapNumber
        , currentDuration = 0.0
        }

width, height, offset, fps :: Int
width = 700

height = 700

offset = 0

fps = 15
