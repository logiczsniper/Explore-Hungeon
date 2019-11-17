module Generators where

import           GameTypes
import           Graphics.Gloss (Picture, blank, pictures, translate)
import           ImageConstants
import           ImageFunctions (getImage)
import           Probability

-- Generate the next image in the map.
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
    nextWall images randomList currentCoords mapNumber mapType gW
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
      otherwise -> nextWater images randomList currentCoords mapNumber
  -- Should not be called
  | otherwise = blank
  where
    sgW = gW - 1
    sgL = gL - 1
    xsgL = gL - 2

-- Generate the coords of the next tile, based on the previous tile coords.
nextCoords :: Coordinates -> Length -> Coordinates
nextCoords current@(x, y) gameLength
  | y == gameLength = (x + 1, 0) -- each time a row is complete, restart in next column.
  | otherwise = (x, y + 1) -- otherwise, increment the row number.

-- Generate the dimensions of the next map, based on the previous map.
nextDimensions :: RandomList -> Int -> Dimensions
nextDimensions randomList mapNumber =
  let validSizes = [x | x <- randomList, x > 10 && x < 30]
   in (validSizes !! mapNumber - 1, validSizes !! mapNumber + 3)

-- Generate the next wall pseudo randomly.
nextWall ::
     PictureList
  -> RandomList
  -> Coordinates
  -> MapNumber
  -> MapType
  -> Width
  -> Picture
nextWall images randomList coordinates@(x, _) mapNumber mapType gameWidth
  | nextProbability' [0 .. 1] = getImage wallSecretOpen images 0
  | nextProbability' [2 .. 4] = getImage wallSecretCracked images 0
  | nextProbability' [5 .. 7] = getImage wallSecretClosed images 0
  | nextProbability' [8 .. 10] = getImage wallStandardDoor images 0
  | nextProbability' [27 .. 35] = getImage wallStandardFancy images 0
  | nextProbability' plantOne = getImage wallPlantsOne images 0
  | nextProbability' plantTwo = getImage wallPlantsTwo images 0
  | nextProbability' plantThree = getImage wallPlantsThree images 0
  | otherwise =
    if nextProbability' [27 .. 45] &&
       mapType /= Wet && x /= 1 && x /= gameWidth - 1
      then pictures
             [ (getImage wallStandardPlain images 0)
             , (translate 0 (-3.5) $ getImage pillar images 0)
             ]
      else getImage wallStandardPlain images 0
  where
    randomInt = getRandomInt randomList coordinates
    nextProbability' = nextProbability randomInt mapNumber
    plantOne =
      case mapType of
        Dry -> [15 .. 16]
        Wet -> [15 .. 18]
    plantTwo =
      case mapType of
        Dry -> [17 .. 18]
        Wet -> [19 .. 22]
    plantThree =
      case mapType of
        Dry -> [19 .. 20]
        Wet -> [23 .. 26]

-- Generate the next floor pseudo randomly.
nextFloor :: PictureList -> RandomList -> Coordinates -> MapNumber -> Picture
nextFloor images randomList coordinates@(x, y) mapNumber
  | nextProbability' [0] = getImage floorDryDoor images 0
  | nextProbability' [1 .. 5] = getImage floorPlantsOne images 0
  | nextProbability' [6 .. 9] = getImage floorPlantsTwo images 0
  | nextProbability' [10 .. 12] = getImage floorPlantsThree images 0
  | nextProbability' [13] && elem (randomList !! x * 10 + y) [1 .. 45] =
    getImage floorDryChest images 0
  | otherwise =
    if nextProbability' [14]
      then pictures
             [ (getImage floorDryPlain images 0)
             , (translate 0 (-3.5) $ getImage barrel images 0)
             ]
      else getImage floorDryPlain images 0
  where
    randomInt = getRandomInt randomList coordinates
    nextProbability' = nextProbability randomInt mapNumber

-- TODO: pass in random int, nextProbability' and curried getImage
nextWater :: PictureList -> RandomList -> Coordinates -> MapNumber -> Picture
nextWater images randomList coordinates@(x, y) mapNumber
  | nextProbability' [0 .. 1] =
    pictures
      [ (getImage floorWetPlain images 0)
      , (translate 0 (-3.5) $ getImage waterBarrel images 0)
      ]
  | otherwise = getImage floorWetPlain images 0
  where
    randomInt = getRandomInt randomList coordinates
    nextProbability' = nextProbability randomInt mapNumber
