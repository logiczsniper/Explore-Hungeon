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

generateMap :: PictureList -> RandomList -> MapNumber -> TileList
generateMap images randomList mapNumber =
  let startingTile =
        Tile
          { picture = blank
          , columnNumber = 0
          , rowNumber = -1
          , isEntrance = False
          }
      mapType = floorsProbability randomList mapNumber
      gameWidth = fst $ nextDimensions randomList
      gameLength = snd $ nextDimensions randomList
      allTiles =
        takeWhile checkTile $
        iterate
          (tileGenerator images randomList mapType (gameWidth, gameLength))
          startingTile
   in map tileAdjust allTiles

checkTile :: Tile -> Bool
checkTile tile =
  if picture tile == blank && rowNumber tile /= -1
    then False
    else True

tileGenerator ::
     PictureList -> RandomList -> MapType -> Dimensions -> Tile -> Tile
tileGenerator images randomList mapType dimensions@(_, gameLength) previousTile =
  let newCoords =
        nextCoords
          (columnNumber previousTile, rowNumber previousTile)
          gameLength
      newPicture = nextPicture images newCoords randomList mapType dimensions
   in Tile
        { picture = newPicture
        , columnNumber = fst newCoords
        , rowNumber = snd newCoords
        , isEntrance =
            elem
              newPicture
              [ (getImage floorDryDoor images 0)
              , (getImage wallSecretOpen images 0)
              , (getImage wallStandardDoor images 0)
              ]
        }

nextPicture ::
     PictureList
  -> Coordinates
  -> RandomList
  -> MapType
  -> Dimensions
  -> Picture
nextPicture images currentCoords@(x, y) randomList mapType (gW, gL)
  -- Border Corners
  | x == 0 && y == 0 = getImage borderCorner images 180
  | x == 0 && y == gL = getImage borderCorner images 270
  | x == gW && y == 0 = getImage borderCorner images 90
  | x == gW && y == gL = getImage borderCorner images 0
  -- Border Sides
  | x == 0 && y > 0 && y < gL = getImage borderSide images 0
  | x == gW && y > 0 && y < gL = getImage borderSide images 180
  | y == 0 && x > 0 && x < gW = getImage borderSide images 270
  | y == gL && x > 0 && x < gW = getImage borderSide images 90
  -- Walls
  | x > 0 && x < gW && y == sgL = nextWall images randomList currentCoords
  -- Floor Corners
  | x == 1 && y == 1 =
    if mapType == Dry
      then getImage floorDryCornerLeft images 0
      else getImage floorWetCornerLeft images 0
  | x == 1 && y == xsgL =
    if mapType == Dry
      then getImage floorDryCornerRight images 180
      else getImage floorWetCornerRight images 180
  | x == sgW && y == 1 =
    if mapType == Dry
      then getImage floorDryCornerRight images 0
      else getImage floorWetCornerRight images 0
  | x == sgW && y == xsgL =
    if mapType == Dry
      then getImage floorDryCornerLeft images 180
      else getImage floorWetCornerLeft images 180
  -- Floor Sides
  | x == 1 && y > 1 && y < xsgL =
    case mapType of
      Dry       -> getImage floorDryHorizontal images 180
      otherwise -> getImage floorWetHorizontal images 180
  | x == sgW && y > 1 && y < xsgL =
    case mapType of
      Dry       -> getImage floorDryHorizontal images 0
      otherwise -> getImage floorWetHorizontal images 0
  | y == 1 && x > 1 && x < sgW =
    case mapType of
      Dry       -> getImage floorDryVertical images 180
      otherwise -> getImage floorWetVertical images 180
  | y == xsgL && x > 1 && x < sgW =
    case mapType of
      Dry       -> getImage floorDryVertical images 0
      otherwise -> getImage floorWetVertical images 0
  -- Floor Internal
  | x > 1 && x < sgW && y > 1 && y < xsgL =
    if mapType == Dry
      then nextFloor images randomList currentCoords
      else getImage floorWetPlain images 0
  -- Should not be called
  | otherwise = blank
  where
    sgW = gW - 1
    sgL = gL - 1
    xsgL = gL - 2

nextCoords :: Coordinates -> Length -> Coordinates
nextCoords current@(x, y) gameLength
  | y == gameLength = (x + 1, 0) -- each time a row is complete, restart in next column.
  | otherwise = (x, y + 1) -- otherwise, increment the row number.

nextDimensions :: RandomList -> Dimensions
nextDimensions randomList =
  let validSizes = [x | x <- randomList, x > 10 && x < 30]
   in (validSizes !! 0, validSizes !! 2)

nextWall :: PictureList -> RandomList -> Coordinates -> Picture
nextWall images randomList coordinates =
  let probability = nextProbability randomList coordinates
   in case probability of
        _
          | elem probability [0 .. 1] -> getImage wallSecretOpen images 0
          | elem probability [2 .. 4] -> getImage wallSecretCracked images 0
          | elem probability [5 .. 9] -> getImage wallSecretClosed images 0
          | elem probability [10 .. 12] -> getImage wallStandardDoor images 0
          | elem probability [13 .. 15] -> getImage wallStandardFancy images 0
          | otherwise -> getImage wallStandardPlain images 0

nextFloor :: PictureList -> RandomList -> Coordinates -> Picture
nextFloor images randomList coordinates =
  let probability = nextProbability randomList coordinates
   in case probability of
        _
          | elem probability [0] -> getImage floorDryDoor images 0
          | elem probability [1 .. 5] -> getImage floorPlantsOne images 0
          | elem probability [6 .. 9] -> getImage floorPlantsTwo images 0
          | elem probability [10 .. 13] -> getImage floorPlantsThree images 0
          | otherwise -> getImage floorDryPlain images 0
