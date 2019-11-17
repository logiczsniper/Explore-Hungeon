module Rendering
  ( render
  ) where

import           GameTypes
import           Graphics.Gloss
import           PointerFunctions (translatePointer)

-- Draw a game state (convert it to a picture).
render :: GameState -> Picture
render game
  -- If the game is over, display game over text.
  | pointerSize <= 0.001 = gameOver
  -- Render menu if mapNumber is 0.
  | mapNumber game == 0 = startMenu
  -- Else, render the game itself: tiles, pointer, shade, score.
  | otherwise =
    pictures $ (createPictures $ tiles game) ++ [pointer, shade, score]
  where
    pointerCoords = coords $ pointerState game
    pointerFrames = frames $ pointerState game
    pointerIndex = index $ pointerState game
    pointerSize = size $ pointerState game
    pointer =
      translatePointer
        (scale pointerSize pointerSize $ pointerFrames !! pointerIndex)
        pointerCoords
    shade = color (getColor pointerSize) (rectangleSolid 10000 10000)
    score =
      color white $
      translate (-200) (330) $
      scale 0.15 0.15 (text ("Score: " ++ (show $ mapNumber game - 1)))
    gameOver =
      translate (-65) 10 $ color white $ scale 0.15 0.15 (text "Game Over")
    startMenu =
      pictures
        [ translate (-65) 10 $ color white $ scale 0.15 0.15 (text "Hungeon")
        , translate (-95) (-15) $
          color white $ scale 0.10 0.10 (text "Press `b` to begin!")
        ]

-- Using the size, get the corresponding alpha value for the shade. Make color with it.
-- Such that when size = 0, alpha = 255 and
--           when size = 1.7, alpha = 0
getColor :: Size -> Color
getColor pointerSize =
  let alpha = round $ pointerSize * (-150) + 255
   in makeColorI 0 0 0 alpha

-- Get the pictures of all tiles.
createPictures :: TileList -> PictureList
createPictures []    = []
createPictures tiles = map (\tile -> picture tile) tiles
