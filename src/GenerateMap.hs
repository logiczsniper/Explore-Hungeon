module GenerateMap
  ( generateMap
  ) where

import qualified Data.List       as List
import qualified Data.Map        as Map
import           GameTypes
import           Graphics.Gloss  (Picture, blank)
import           ImageConstants
import           ImageFunctions
import           System.Random
import           TileTranslation

{-
STEPS:

1) Randomly generate two integers between 5 and 45- these are the
    length and width of the floor.
2) From those dimensions, create a picture object from pictures PictureList where
    each Picture is a tile that has been translated. This is single picture is all the floors.
3) Using the TileList of floor tiles, create a TileList for the wall tiles.
4) Using the TileList of wall tiles, create a TileList for the border tiles.

Method two:
In function which iterates through each row and column entry,
starting at C0R0 and ending at C24R24, write an algorithm which takes the previous tiles and
outputs the next tile! Use large if statements or case guards etc.
-}
{-
Recursively iterates through all of the columns and rows,
for each value calling the algorithm function.
Takes all pictures, the random generator and the current coordinates.
-}
generateMap :: PictureList -> StdGen -> TileList
generateMap images generator =
  let startingTile = Tile {picture = blank, columnNumber = -1, rowNumber = -1}
      allTiles = take (25 * 25) $ iterate (tileGenerator images) startingTile
   in map tileTranslate allTiles

tileGenerator :: PictureList -> Tile -> Tile
tileGenerator images previousTile =
  let newCoords = nextCoords (columnNumber previousTile, rowNumber previousTile)
      newPicture = nextPicture images newCoords
   in Tile
        { picture = newPicture
        , columnNumber = fst newCoords
        , rowNumber = snd newCoords
        }

nextPicture :: PictureList -> Coordinates -> Picture
nextPicture images currentCoords =
  case currentCoords of
    (0, 0) -> getPictureFromConstant borderTopRight images
    _      -> blank

nextProbability :: StdGen -> Int
nextProbability generator = fst $ randomR (1, 10) generator

nextCoords :: Coordinates -> Coordinates
nextCoords current@(x, y) =
  case current of
    (_, 24) -> (x + 1, 0) -- each time a row is complete, restart in next column.
    (_, _)  -> (x, y + 1) -- otherwise, increment the row number.
    {-   let previousTiles =
        previousTiles ++ [nextTile images generator currentCoords previousTiles]
   in generateMap images generator (nextCoords currentCoords) previousTiles -}
{- The function starts with index = 0 and previousTiles = [] -}
{- generateMap :: PictureList -> StdGen -> Coordinates -> TileList -> TileList
generateMap _ _ (24, 24) _ = []
generateMap images generator (0, 0) previousTiles =
generateMap images generator currentCoords previousTiles =
  previousTiles ++
  generateMap images generator (nextCoords currentCoords) previousTiles -}
{- nextTile :: PictureList -> StdGen -> Coordinates -> TileList -> Tile
nextTile images generator currentCoords previousTiles =
  Tile
    { picture = nextPicture images currentCoords
    , columnNumber = fst currentCoords
    , rowNumber = snd currentCoords
    } -}
    {- generateFloorDimensions :: StdGen -> Coordinates
generateFloorDimensions generator =
    (fst $ randomR (5, 45) generator, fst $ randomR (5, 45) generator)
-}
