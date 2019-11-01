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

generateMap :: PictureList -> StdGen -> TileList
generateMap images generator =
  let startingTile = Tile {picture = blank, columnNumber = 0, rowNumber = -1}
      floorTile = floorsProbability generator
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
    (0, 0) -> getImage borderCorner images 180
    (0, 24) -> getImage borderCorner images 270
    (24, 0) -> getImage borderCorner images 90
    (24, 24) -> getImage borderCorner images 0
    (1, 1)
      | floorTile == Dry -> getImage floorDryCornerLeft images 0
      | otherwise -> getImage floorWetCornerLeft images 0
    (1, 22)
      | floorTile == Dry -> getImage floorDryCornerRight images 180
      | otherwise -> getImage floorWetCornerRight images 180
    (23, 1)
      | floorTile == Dry -> getImage floorDryCornerRight images 0
      | otherwise -> getImage floorWetCornerRight images 0
    (23, 22)
      | floorTile == Dry -> getImage floorDryCornerLeft images 180
      | otherwise -> getImage floorWetCornerLeft images 180
    (_, _)
      | y > 0 && y < 24 && x == 0 -> getImage borderSide images 0
      | y > 0 && y < 24 && x == 24 -> getImage borderSide images 180
      | x > 0 && x < 24 && y == 0 -> getImage borderSide images 270
      | x > 0 && x < 24 && y == 24 -> getImage borderSide images 90
      | x > 0 && x < 24 && y == 23 -> getImage wallStandardPlain images 0
      | x == 1 && y > 1 && y < 22 ->
        case floorTile of
          Dry       -> getImage floorDryHorizontal images 180
          otherwise -> getImage floorWetHorizontal images 180
      | x == 23 && y > 1 && y < 22 ->
        case floorTile of
          Dry       -> getImage floorDryHorizontal images 0
          otherwise -> getImage floorWetHorizontal images 0
      | y == 22 && x > 1 && x < 23 ->
        case floorTile of
          Dry       -> getImage floorDryVertical images 0
          otherwise -> getImage floorWetVertical images 0
      | y == 1 && x > 1 && x < 23 ->
        case floorTile of
          Dry       -> getImage floorDryVertical images 180
          otherwise -> getImage floorWetVertical images 180
      | x > 1 && x < 23 && y > 1 && y < 22 && floorTile == Dry ->
        pickNextFloorTile images generator
      | x > 1 && x < 23 && y > 1 && y < 22 && floorTile == Wet ->
        getImage floorWetPlain images 0
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
        1 -> getImage floorDryDoor images 0
        _
          | elem probability [2 .. 5] -> getImage floorPlantsOne images 0
          | elem probability [6 .. 9] -> getImage floorPlantsTwo images 0
          | elem probability [10 .. 13] -> getImage floorPlantsThree images 0
          | otherwise -> getImage floorDryPlain images 0
