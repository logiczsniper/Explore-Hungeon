module Main where

import           Sound.ProteaAudio
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Lib
import           System.Environment

main :: IO ()
main = do
  images <- allImages
  play window
       backgroundColour
       fps
       (initialState images)
       render
       handleEvent
       update

width, height, offset, fps :: Int
-- 50 * 50 tiles at 16px each
width = 800
height = 800
offset = 100
fps = 25

imagePaths :: [ImagePath]
imagePaths =
  [ buildImagePath (borderPath "bottom")         1
  , buildImagePath (borderPath "top")            2
  , buildImagePath (borderPath "right")          3
  , buildImagePath (borderPath "left")           4
  , buildImagePath (borderPath "topLeft")        5
  , buildImagePath (borderPath "topRight")       6
  , buildImagePath (borderPath "bottomRight")    7
  , buildImagePath (borderPath "bottomLeft")     9
  , buildImagePath (floorPath "dry/bottom")      10
  , buildImagePath (floorPath "dry/top")         11
  , buildImagePath (floorPath "dry/right")       12
  , buildImagePath (floorPath "dry/left")        13
  , buildImagePath (floorPath "dry/topLeft")     14
  , buildImagePath (floorPath "dry/topRight")    15
  , buildImagePath (floorPath "dry/bottomRight") 16
  , buildImagePath (floorPath "dry/bottomLeft")  17
  , buildImagePath (floorPath "dry/door")        18
  , buildImagePath (floorPath "dry/plain")       19
  , buildImagePath (floorPath "wet/bottom")      20
  , buildImagePath (floorPath "wet/top")         21
  , buildImagePath (floorPath "wet/right")       22
  , buildImagePath (floorPath "wet/left")        23
  , buildImagePath (floorPath "wet/topLeft")     24
  , buildImagePath (floorPath "wet/topRight")    25
  , buildImagePath (floorPath "wet/bottomRight") 26
  , buildImagePath (floorPath "wet/bottomLeft")  27
  , buildImagePath (floorPath "wet/plain")       28
  , buildImagePath (floorPath "plants/one")      29
  , buildImagePath (floorPath "plants/two")      30
  , buildImagePath (floorPath "plants/three")    31
  , buildImagePath (wallPath "secret/closed")    32
  , buildImagePath (wallPath "secret/cracked")   33
  , buildImagePath (wallPath "secret/open")      34
  , buildImagePath (wallPath "standard/boarded") 35
  , buildImagePath (wallPath "standard/door")    36
  , buildImagePath (wallPath "standard/fancy")   37
  , buildImagePath (wallPath "standard/plain")   38
  , buildImagePath (wallPath "plants/one")       39
  , buildImagePath (wallPath "plants/two")       40
  , buildImagePath (wallPath "plants/three")     41
  ]

allImages :: IO [Picture]
allImages = mapM loadBMP $ map path imagePaths

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

data ImagePath = ImagePath {
  path :: String
  , imageId :: Int
}

buildImagePath :: String -> Int -> ImagePath
buildImagePath newPath newId = ImagePath { path = newPath, imageId = newId }

getPictureFromIndex :: Int -> [Picture] -> Picture
getPictureFromIndex index images = images !! index

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

getBasicFloorImages :: [Picture] -> [Picture]
getBasicFloorImages images = []

generateMap :: [Picture] -> [Tile]
generateMap images = []

-- | Initialize the game with this game state.
initialState :: [Picture] -> GameState
initialState images =
  let startingTiles = generateMap images
  in  Game { tiles = startingTiles, effects = [] }
