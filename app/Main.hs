module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Sound.ProteaAudio

import           GameTypes
import           GenerateMap
import           ImageFunctions
import           ImagePathHelpers
import           PointerFunctions
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
  pointerImage <- getPointer
  images <- allImages
  generator <- getStdGen
  play
    window
    backgroundColour
    fps
    (initialState images $ randomRs (0, 99) generator)
    (render pointerImage)
    (handleEvent images $ randomRs (0, 99) generator)
    update

-- | Some defining helper main functions. Note: 50 * 50 tiles at 16px each max.
width, height, offset, fps :: Int
width = 700

height = 700

offset = 0

fps = 25

window :: Display
window = InWindow "Explore" (width, height) (offset, offset)

backgroundColour :: Color
backgroundColour = makeColorI 19 19 19 255

-- | Handle the mouse input from the user, changing the game state.
handleEvent :: PictureList -> RandomList -> Event -> GameState -> GameState
handleEvent images randomList key initState =
  case key of
    EventKey (SpecialKey KeyUp) Down _ _ -> movePointer initState (x, y + 1)
    EventKey (SpecialKey KeyDown) Down _ _ -> movePointer initState (x, y - 1)
    EventKey (SpecialKey KeyLeft) Down _ _ -> movePointer initState (x - 1, y)
    EventKey (SpecialKey KeyRight) Down _ _ -> movePointer initState (x + 1, y)
    EventKey (Char 'e') Down _ _ ->
      tilePointerInteraction coords initState images randomList
    otherwise -> initState
  where
    coords = pointerCoords initState
    x = fst coords
    y = snd coords

-- | Update the game state.
update :: Float -> GameState -> GameState
update time initState = initState

-- | Draw a game state (convert it to a picture).
render :: Picture -> GameState -> Picture
render pointerPicture game =
  pictures $
  (createPictures $ tiles game) ++ [translatePointer pointerPicture coords]
  where
    coords = pointerCoords game

createPictures :: TileList -> PictureList
createPictures []    = []
createPictures tiles = map (\tile -> picture tile) tiles

-- | Initialize the game with this game state.
initialState :: PictureList -> RandomList -> GameState
initialState images randomList =
  let startingMapNumber = 0
      startingTiles = generateMap images randomList startingMapNumber
   in GameState
        { tiles = startingTiles
        , effects = []
        , pointerCoords = (20, 20)
        , mapNumber = startingMapNumber
        }
