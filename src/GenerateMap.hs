module GenerateMap
  ( generateMap
  ) where

import qualified Data.List       as List
import qualified Data.Map        as Map
import           GameTypes
import           Graphics.Gloss  (Picture, blank, pictures)
import           ImageConstants
import           ImageFunctions
import           Probability
import           System.Random
import           TileRotation
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
  let startingTile =
        Tile {picture = blank, columnNumber = 0, rowNumber = -1, rotation = 0}
      floorTile = setFloor generator
      allTiles =
        take (25 * 25 + 1) $
        iterate (tileGenerator images generator floorTile) startingTile
   in map tileRotate $ map tileTranslate allTiles

tileGenerator :: PictureList -> StdGen -> FloorType -> Tile -> Tile
tileGenerator images generator floorTile previousTile =
  let newCoords = nextCoords (columnNumber previousTile, rowNumber previousTile)
      newPicture = nextPicture images newCoords generator floorTile
      newRotation = getRotationFromPicture newPicture images
   in Tile
        { picture = newPicture
        , columnNumber = fst newCoords
        , rowNumber = snd newCoords
        , rotation = newRotation
        }

nextPicture :: PictureList -> Coordinates -> StdGen -> FloorType -> Picture
nextPicture images currentCoords@(x, y) generator floorTile =
  case currentCoords of
    (0, 0) -> getPictureFromConstant borderTopRight images
    (0, 24) -> getPictureFromConstant borderBottomRight images
    (24, 0) -> getPictureFromConstant borderTopLeft images
    (24, 24) -> getPictureFromConstant borderBottomLeft images
    (1, 22)
      | floorTile == Dry -> getPictureFromConstant floorDryTopLeft images
      | otherwise -> getPictureFromConstant floorWetTopLeft images
    (23, 22)
      | floorTile == Dry -> getPictureFromConstant floorDryTopRight images
      | otherwise -> getPictureFromConstant floorWetTopRight images
    (_, _)
      | y > 0 && y < 24 && x == 0 -> getPictureFromConstant borderRight images
      | y > 0 && y < 24 && x == 24 -> getPictureFromConstant borderLeft images
      | x > 0 && x < 24 && y == 0 -> getPictureFromConstant borderTop images
      | x > 0 && x < 24 && y == 24 -> getPictureFromConstant borderBottom images
      | x > 0 && x < 24 && y == 23 ->
        getPictureFromConstant wallStandardPlain images
      | x == 1 && y > 0 && y < 22 ->
        case floorTile of
          Dry       -> getPictureFromConstant floorDryLeft images
          otherwise -> getPictureFromConstant floorWetLeft images
      | x == 23 && y > 0 && y < 22 ->
        case floorTile of
          Dry       -> getPictureFromConstant floorDryRight images
          otherwise -> getPictureFromConstant floorWetRight images
      | y == 22 && x > 1 && x < 23 ->
        case floorTile of
          Dry       -> getPictureFromConstant floorDryTop images
          otherwise -> getPictureFromConstant floorWetTop images
      | x > 1 && x < 23 && y >= 1 && y < 22 && floorTile == Dry ->
        pickNextFloorTile images generator
      | x > 1 && x < 23 && y > 1 && y < 22 && floorTile == Wet ->
        getPictureFromConstant floorWetPlain images
      | otherwise -> blank

nextCoords :: Coordinates -> Coordinates
nextCoords current@(x, y) =
  case current of
    (_, 24) -> (x + 1, 0) -- each time a row is complete, restart in next column.
    (_, _)  -> (x, y + 1) -- otherwise, increment the row number.

pickNextFloorTile :: PictureList -> StdGen -> Picture
pickNextFloorTile images generator =
  let probability = nextProbability generator 40
   in case probability of
        1 -> getPictureFromConstant floorDryDoor images
        _
          | elem probability [2 .. 5] ->
            getPictureFromConstant floorPlantsOne images
          | elem probability [6 .. 9] ->
            getPictureFromConstant floorPlantsTwo images
          | elem probability [10 .. 13] ->
            getPictureFromConstant floorPlantsThree images
          | otherwise -> getPictureFromConstant floorDryPlain images

setFloor :: StdGen -> FloorType
setFloor generator = floorsProbability generator
