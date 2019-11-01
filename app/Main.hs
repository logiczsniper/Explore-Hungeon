module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Sound.ProteaAudio

import           GameTypes
import           GenerateMap
import           ImageFunctions

import           System.Environment
import           System.Random

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
main :: IO ()
main = do
  images <- allImages
  generator <- getStdGen
  play
    window
    backgroundColour
    fps
    (initialState images generator)
    render
    handleEvent
    update

-- | Some defining helper main functions. Note: 50 * 50 tiles at 16px each max.
width, height, offset, fps :: Int
width = 800

height = 800

offset = 100

fps = 25

window :: Display
window = InWindow "Explore" (width, height) (offset, offset)

backgroundColour :: Color
backgroundColour = makeColorI 19 19 19 255

-- | Handle the mouse input from the user, changing the game state.
handleEvent :: Event -> GameState -> GameState
handleEvent event initState = initState

-- | Update the game state.
update :: Float -> GameState -> GameState
update time initState = initState

-- | Draw a game state (convert it to a picture).
render :: GameState -> Picture
render game = pictures $ createPictures $ tiles game

createPictures :: TileList -> PictureList
createPictures []    = []
createPictures tiles = map (\tile -> picture tile) tiles

-- | Initialize the game with this game state.
initialState :: PictureList -> StdGen -> GameState
initialState images generator =
  let startingTiles = generateMap images generator
   in Game {tiles = startingTiles, effects = []}
