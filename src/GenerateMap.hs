module GenerateMap
  ( generateMap
  ) where

import qualified Data.List      as List
import qualified Data.Map       as Map
import           GameTypes
import           Generators
import           Graphics.Gloss (Picture, blank, pictures)
import           ImageConstants
import           ImageFunctions (getImage)
import           Probability    (floorsProbability)
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
      gameWidth = fst $ nextDimensions randomList mapNumber
      gameLength = snd $ nextDimensions randomList mapNumber
      allTiles =
        takeWhile checkTile $
        iterate
          (tileGenerator
             images
             randomList
             mapType
             mapNumber
             (gameWidth, gameLength))
          startingTile
   in map tileAdjust allTiles

checkTile :: Tile -> Bool
checkTile tile =
  if picture tile == blank && rowNumber tile /= -1
    then False
    else True

tileGenerator ::
     PictureList
  -> RandomList
  -> MapType
  -> MapNumber
  -> Dimensions
  -> Tile
  -> Tile
tileGenerator images randomList mapType mapNumber dimensions@(_, gameLength) previousTile =
  let newCoords =
        nextCoords
          (columnNumber previousTile, rowNumber previousTile)
          gameLength
      newPicture =
        nextPicture images newCoords randomList mapType dimensions mapNumber
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
