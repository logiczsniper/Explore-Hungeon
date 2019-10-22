module GenerateMap
    ( generateMap
    )
where

import           Graphics.Gloss                 ( Picture )
import           GameTypes                      ( Tile )
import           System.Random
import           ImageFunctions
import qualified Data.List                      as  List
import qualified Data.Map                       as  Map

{-
STEPS:

1) Randomly generate two integers between 5 and 45- these are the
    length and width of the floor.
2) From those dimensions, create a picture object from pictures [Picture] where
    each Picture is a tile that has been translated. This is single picture is all the floors.
3) Using the [Tile] of floor tiles, create a [Tile] for the wall tiles.
4) Using the [Tile] of wall tiles, create a [Tile] for the border tiles.

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
generateMap :: [Picture] -> StdGen -> (Int, Int) -> [Tile]
generateMap images generator current = if current == (24, 24)
                                              then []
                                              else let newCoords = nextCoords current
                                                   in generateMap images generator newCoords

nextTile :: [Picture] -> StdGen -> (Int, Int) -> [Tile] -> Tile
nextTile images generator currentCoords previousTiles = if currentCoords == (0, 0) && elem (nextProbability generator) [1..4]    -- There is a 40% chance of this.
                                                        then Tile { picture = 
                                                                  , columnNumber = fst currentCoords
                                                                  , rowNumber :: snd currentCoords
                                                                  }                                                    

nextProbability :: StdGen -> Int
nextProbability generator = randomR (1, 10) generator

nextCoords :: (Int, Int) -> (Int, Int)
nextCoords current@(x, y) = case current of 
                                (24, 24) -> (25, 25)    -- If this is returned, STOP recusion!
                                (_, 24) -> (x + 1, 0)   -- each time a row is complete, restart in next column.
                                (_, _) -> (x, y + 1)    -- otherwise, increment the row number.



{- generateFloorDimensions :: StdGen -> (Int, Int)
generateFloorDimensions generator =
    (fst $ randomR (5, 45) generator, fst $ randomR (5, 45) generator)

getBasicFloorImages :: [Picture] -> StdGen -> [Picture]
getBasicFloorImages images generator =
    let dimensions = generateFloorDimensions generator
    in  createFloorPictures dimensions images

{- Gives you a list of pictures that you need to make the floor -}
createFloorPictures :: (Int, Int) -> [Picture] -> [Picture]
createFloorPictures dimensions images = 
    [ getPictureFromIndex 14 images
    , getPictureFromIndex 15 images
    , getPictureFromIndex 16 images
    , getPictureFromIndex 17 images
    ]
    ++ (List.take (x - 2) $ repeat (getPictureFromIndex 10 images))
    ++ (List.take (x - 2) $ repeat (getPictureFromIndex 11 images))
    ++ (List.take (y - 2) $ repeat (getPictureFromIndex 12 images))
    ++ (List.take (y - 2) $ repeat (getPictureFromIndex 13 images))
    where   x = fst dimensions
            y = snd dimensions

createFloorTiles :: [Picture] -> [Tile]
createFloorTiles floorPictures = []
 -}
