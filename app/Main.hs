module Main where

import           Sound.ProteaAudio
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Lib
import           System.Environment

main :: IO ()
main = do
  grass <- loadBMP $ imagePaths !! 0
  play window
       backgroundColour
       fps
       (initialState grass)
       render
       handleEvent
       update

width, height, offset, fps :: Int
-- 50 * 50 tiles at 16px each
width = 800
height = 800
offset = 100
fps = 25

imagePaths :: [String]
imagePaths =
  [ borderPath "bottom"
  , borderPath "top"
  , borderPath "right"
  , borderPath "left"
  , borderPath "topLeft"
  , borderPath "topRight"
  , borderPath "bottomRight"
  , borderPath "bottomLeft"
  , floorPath "dry/bottom"
  , floorPath "dry/top"
  , floorPath "dry/right"
  , floorPath "dry/left"
  , floorPath "dry/topLeft"
  , floorPath "dry/topRight"
  , floorPath "dry/bottomRight"
  , floorPath "dry/bottomLeft"
  , floorPath "dry/door"
  , floorPath "dry/plain"
  , floorPath "wet/bottom"
  , floorPath "wet/top"
  , floorPath "wet/right"
  , floorPath "wet/left"
  , floorPath "wet/topLeft"
  , floorPath "wet/topRight"
  , floorPath "wet/bottomRight"
  , floorPath "wet/bottomLeft"
  , floorPath "wet/plain"
  , floorPath "plants/one"
  , floorPath "plants/two"
  , floorPath "plants/three"
  , wallPath "secret/closed"
  , wallPath "secret/cracked"
  , wallPath "secret/open"
  , wallPath "standard/boarded"
  , wallPath "standard/door"
  , wallPath "standard/fancy"
  , wallPath "standard/plain"
  , wallPath "plants/one"
  , wallPath "plants/two"
  , wallPath "plants/three"
  ]

generatePath :: String -> String -> String
generatePath subDirectory name =
  "assets/images/" ++ subDirectory ++ "/" ++ name ++ ".bmp"

borderPath :: String -> String
borderPath = generatePath "borders"

floorPath :: String -> String
floorPath = generatePath "floors"

wallPath :: String -> String
wallPath = generatePath "walls"

window :: Display
window = InWindow "Explore" (width, height) (offset, offset)

backgroundColour :: Color
backgroundColour = makeColorI 19 19 19 255

-- TODO: pan the viewport as user interaction
handleEvent :: Event -> GameState -> GameState
handleEvent event initState = initState

update :: Float -> GameState -> GameState
update time initState = initState

-- | A data structure to hold the state of the game.
data GameState = Game
  { tiles :: [Tile]
  , effects :: [Effect]
  } deriving Show

data Effect = Effect
  { frames :: [Picture]
    , frameCount :: Integer
  } deriving Show

data Tile = Tile
  { picture :: Picture
  , columnNumber :: Integer
  , rowNumber :: Integer
  } deriving Show

-- | Draw a game state (convert it to a picture).
render :: GameState -> Picture
render game = pictures $ createPictures $ tiles game

calculateTranslationX :: Integer -> Float
calculateTranslationX columnNumber = fromInteger columnNumber * 16.0 + 4

calculateTranslationY :: Integer -> Float
calculateTranslationY rowNumber = fromInteger rowNumber * 16.0 + 4

tileTranslate :: Tile -> Picture
tileTranslate tile =
  translate (calculateTranslationX $ columnNumber tile)
            (calculateTranslationY $ rowNumber tile)
    $ picture tile

createPictures :: [Tile] -> [Picture]
createPictures []    = []
createPictures tiles = map tileTranslate tiles

-- | Initialize the game with this game state.
initialState :: Picture -> GameState
initialState grass =
  let onlyTile = Tile { picture = grass, columnNumber = 3, rowNumber = 5 }
  in  Game { tiles = [onlyTile], effects = [] }
