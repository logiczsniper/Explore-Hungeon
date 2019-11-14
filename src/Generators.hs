module Generators where

import           GameTypes
import           Graphics.Gloss (Picture, blank)
import           ImageConstants
import           ImageFunctions (getImage)
import Probability

nextPicture ::
     PictureList
  -> Coordinates
  -> RandomList
  -> MapType
  -> Dimensions
  -> MapNumber
  -> Picture
nextPicture images currentCoords@(x, y) randomList mapType (gW, gL) mapNumber
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
  | x > 0 && x < gW && y == sgL =
    nextWall images randomList currentCoords mapNumber
  -- Floor Corners
  | x == 1 && y == 1 =
    case mapType of
      Dry       -> getImage floorDryCornerLeft images 0
      otherwise -> getImage floorWetCornerLeft images 0
  | x == 1 && y == xsgL =
    case mapType of
      Dry       -> getImage floorDryCornerRight images 180
      otherwise -> getImage floorWetCornerRight images 180
  | x == sgW && y == 1 =
    case mapType of
      Dry       -> getImage floorDryCornerRight images 0
      otherwise -> getImage floorWetCornerRight images 0
  | x == sgW && y == xsgL =
    case mapType of
      Dry       -> getImage floorDryCornerLeft images 180
      otherwise -> getImage floorWetCornerLeft images 180
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
    case mapType of
      Dry       -> nextFloor images randomList currentCoords mapNumber
      otherwise -> getImage floorWetPlain images 0
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

nextDimensions :: RandomList -> Int -> Dimensions
nextDimensions randomList mapNumber =
  let validSizes = [x | x <- randomList, x > 10 && x < 30]
   in (validSizes !! mapNumber - 1, validSizes !! mapNumber + 3)

nextWall :: PictureList -> RandomList -> Coordinates -> MapNumber -> Picture
nextWall images randomList coordinates mapNumber
  | nextProbability' [0 .. 1] = getImage wallSecretOpen images 0
  | nextProbability' [2 .. 4] = getImage wallSecretCracked images 0
  | nextProbability' [5 .. 9] = getImage wallSecretClosed images 0
  | nextProbability' [10 .. 12] = getImage wallStandardDoor images 0
  | nextProbability' [13 .. 15] = getImage wallStandardFancy images 0
  | otherwise = getImage wallStandardPlain images 0
  where
    randomInt = getRandomInt randomList coordinates
    nextProbability' = nextProbability randomInt mapNumber

nextFloor :: PictureList -> RandomList -> Coordinates -> MapNumber -> Picture
nextFloor images randomList coordinates mapNumber
  | nextProbability' [0] = getImage floorDryDoor images 0
  | nextProbability' [1 .. 5] = getImage floorPlantsOne images 0
  | nextProbability' [6 .. 9] = getImage floorPlantsTwo images 0
  | nextProbability' [10 .. 13] = getImage floorPlantsThree images 0
  | otherwise = getImage floorDryPlain images 0
  where
    randomInt = getRandomInt randomList coordinates
    nextProbability' = nextProbability randomInt mapNumber
