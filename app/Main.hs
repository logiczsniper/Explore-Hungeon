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
  frames <- getFrames
  images <- allImages
  generator <- getStdGen
  play
    window
    backgroundColour
    fps
    (initialState images frames $ randomRs (0, 99) generator)
    render
    (handleEvent images $ randomRs (0, 99) generator)
    update

-- | Some defining helper main functions. Note: 50 * 50 tiles at 16px each max.
width, height, offset, fps :: Int
width = 700

height = 700

offset = 0

fps = 15

window :: Display
window = InWindow "Explore" (width, height) (offset, offset)

backgroundColour :: Color
backgroundColour = makeColorI 19 19 19 255

-- | Handle the mouse input from the user, changing the game state.
handleEvent :: PictureList -> RandomList -> Event -> GameState -> GameState
handleEvent images randomList key initState =
  case key of
    EventKey (Char 'w') Down _ _ -> movePointer initState (x, y + 1)
    EventKey (Char 's') Down _ _ -> movePointer initState (x, y - 1)
    EventKey (Char 'a') Down _ _ -> movePointer initState (x - 1, y)
    EventKey (Char 'd') Down _ _ -> movePointer initState (x + 1, y)
    EventKey (Char 'q') Down _ _ -> error "Player has quit."
    EventKey (Char 'e') Down _ _ ->
      tilePointerInteraction pointerCoords initState images randomList
    otherwise -> initState
  where
    pointerCoords = coords $ pointerState initState
    x = fst pointerCoords
    y = snd pointerCoords

-- | Update the game state.
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

incrementIndex :: Int -> Int -> Int
incrementIndex x 0 = x
incrementIndex startIndex increment
  | startIndex == 5 = incrementIndex 0 (increment - 1)
  | startIndex < 5 = incrementIndex (startIndex + 1) (increment - 1)

updateSize :: Size -> MapNumber -> Size
updateSize x mapNumber
  | x <= 0 = 0
  | otherwise = x - (0.001 + (fromIntegral mapNumber) * 0.0012)

-- | Draw a game state (convert it to a picture).
render :: GameState -> Picture
render game =
  pictures $
  (createPictures $ tiles game) ++
  [ translatePointer
      (scale pointerSize pointerSize $ pointerFrames !! pointerIndex)
      pointerCoords
  ]
  where
    pointerCoords = coords $ pointerState game
    pointerFrames = frames $ pointerState game
    pointerIndex = index $ pointerState game
    pointerSize = size $ pointerState game

createPictures :: TileList -> PictureList
createPictures []    = []
createPictures tiles = map (\tile -> picture tile) tiles

-- | Initialize the game with this game state.
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
