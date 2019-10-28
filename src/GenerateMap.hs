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
  let startingTile = Tile {picture = blank, columnNumber = 0, rowNumber = -1}
      allTiles =
        take (25 * 25 + 1) $
        iterate (tileGenerator images generator) startingTile
   in map tileTranslate allTiles

tileGenerator :: PictureList -> StdGen -> Tile -> Tile
tileGenerator images generator previousTile =
  let newCoords = nextCoords (columnNumber previousTile, rowNumber previousTile)
      newPicture = nextPicture images newCoords generator
   in Tile
        { picture = newPicture
        , columnNumber = fst newCoords
        , rowNumber = snd newCoords
        }

nextPicture :: PictureList -> Coordinates -> StdGen -> Picture
nextPicture images currentCoords@(x, y) generator =
  case currentCoords of
    (0, 0) -> getPictureFromConstant borderTopRight images
    (0, 24) -> getPictureFromConstant borderBottomRight images
    (24, 0) -> getPictureFromConstant borderTopLeft images
    (24, 24) -> getPictureFromConstant borderBottomLeft images
    (_, _) ->
      if y > 0 && y < 24 && x == 0
        then getPictureFromConstant borderRight images
        else if y > 0 && y < 24 && x == 24
               then getPictureFromConstant borderLeft images
               else if x > 0 && x < 24 && y == 0
                      then getPictureFromConstant borderTop images
                      else if x > 0 && x < 24 && y == 24
                             then getPictureFromConstant borderBottom images
                             else if x > 1 && x < 23 && y > 1 && y < 23
                                    then let floorType = setFloor generator
                                          in if floorType == Dry
                                               then getPictureFromConstant
                                                      floorDryBottomLeft
                                                      images
                                               else getPictureFromConstant
                                                      floorWetBottomLeft
                                                      images
                                    else blank

nextCoords :: Coordinates -> Coordinates
nextCoords current@(x, y) =
  case current of
    (_, 24) -> (x + 1, 0) -- each time a row is complete, restart in next column.
    (_, _)  -> (x, y + 1) -- otherwise, increment the row number.

setFloor :: StdGen -> FloorType
setFloor generator = floorsProbability generator
