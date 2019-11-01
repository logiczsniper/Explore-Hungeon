module GenerateMap
  ( generateMap
  ) where

import qualified Data.List      as List
import qualified Data.Map       as Map
import           GameTypes
import           Graphics.Gloss (Picture, blank, pictures)
import           ImageConstants
import           ImageFunctions
import           Probability
import           System.Random
import           TileAdjust

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
  let startingTile = Tile {picture = blank, columnNumber = 0, rowNumber = -1}
      floorTile = setFloor generator
      allTiles =
        take (25 * 25 + 1) $
        iterate (tileGenerator images generator floorTile) startingTile
   in map tileAdjust allTiles

tileGenerator :: PictureList -> StdGen -> FloorType -> Tile -> Tile
tileGenerator images generator floorTile previousTile =
  let newCoords = nextCoords (columnNumber previousTile, rowNumber previousTile)
      newPicture = nextPicture images newCoords generator floorTile
   in Tile
        { picture = newPicture
        , columnNumber = fst newCoords
        , rowNumber = snd newCoords
        }

nextPicture :: PictureList -> Coordinates -> StdGen -> FloorType -> Picture
nextPicture images currentCoords@(x, y) generator floorTile =
  case currentCoords of
    (0, 0) -> getPictureFromConstant borderCorner images 180
    (0, 24) -> getPictureFromConstant borderCorner images 270
    (24, 0) -> getPictureFromConstant borderCorner images 90
    (24, 24) -> getPictureFromConstant borderCorner images 0
    (1, 22)
      | floorTile == Dry ->
        getPictureFromConstant floorDryCornerRight images 180
      | otherwise -> getPictureFromConstant floorWetCornerRight images 180
    (23, 22)
      | floorTile == Dry -> getPictureFromConstant floorDryCornerLeft images 180
      | otherwise -> getPictureFromConstant floorWetCornerLeft images 180
    (_, _)
      | y > 0 && y < 24 && x == 0 -> getPictureFromConstant borderSide images 0
      | y > 0 && y < 24 && x == 24 ->
        getPictureFromConstant borderSide images 180
      | x > 0 && x < 24 && y == 0 ->
        getPictureFromConstant borderSide images 270
      | x > 0 && x < 24 && y == 24 ->
        getPictureFromConstant borderSide images 90
      | x > 0 && x < 24 && y == 23 ->
        getPictureFromConstant wallStandardPlain images 0
      | x == 1 && y > 0 && y < 22 ->
        case floorTile of
          Dry       -> getPictureFromConstant floorDryHorizontal images 180
          otherwise -> getPictureFromConstant floorWetHorizontal images 180
      | x == 23 && y > 0 && y < 22 ->
        case floorTile of
          Dry       -> getPictureFromConstant floorDryHorizontal images 0
          otherwise -> getPictureFromConstant floorWetHorizontal images 0
      | y == 22 && x > 1 && x < 23 ->
        case floorTile of
          Dry       -> getPictureFromConstant floorDryVertical images 0
          otherwise -> getPictureFromConstant floorWetVertical images 0
      | x > 1 && x < 23 && y >= 1 && y < 22 && floorTile == Dry ->
        pickNextFloorTile images generator
      | x > 1 && x < 23 && y >= 1 && y < 22 && floorTile == Wet ->
        getPictureFromConstant floorWetPlain images 0
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
        1 -> getPictureFromConstant floorDryDoor images 0
        _
          | elem probability [2 .. 5] ->
            getPictureFromConstant floorPlantsOne images 0
          | elem probability [6 .. 9] ->
            getPictureFromConstant floorPlantsTwo images 0
          | elem probability [10 .. 13] ->
            getPictureFromConstant floorPlantsThree images 0
          | otherwise -> getPictureFromConstant floorDryPlain images 0

setFloor :: StdGen -> FloorType
setFloor generator = floorsProbability generator
