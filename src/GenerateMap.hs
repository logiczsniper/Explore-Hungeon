module GenerateMap
    ( generateMap
    )
where

import           Graphics.Gloss                 ( Picture, blank)
import           GameTypes                      
import           System.Random
import           ImageFunctions
import           ImageConstants
import qualified Data.List                      as  List
import qualified Data.Map                       as  Map

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
{- generateMap :: PictureList -> StdGen -> Coordinates -> TileList
generateMap images generator current = if current == (24, 24)
                                              then []
                                              else let newCoords = nextCoords current
                                                   in generateMap images generator newCoords -}

{- TODO: make recursive -}

{- The function starts with index = 0 and previousTiles = [] -}
generateMap :: PictureList -> StdGen -> Coordinates -> TileList -> Int -> TileList
generateMap images generator currentCoords previousTiles index 
    | index < 25 * 25   =   let previousTiles = previousTiles ++ [nextTile images generator currentCoords previousTiles]
                            in generateMap images generator currentCoords previousTiles $ index + 1
    | otherwise         = previousTiles
                                                

nextTile :: PictureList -> StdGen -> Coordinates -> TileList -> Tile
nextTile images generator currentCoords previousTiles = Tile    { picture = nextPicture images currentCoords
                                                                , columnNumber = fst currentCoords
                                                                , rowNumber = snd currentCoords
                                                                }

nextPicture :: PictureList -> Coordinates -> Picture
nextPicture images currentCoords = case currentCoords of
    (0, 0)  -> getPictureFromIndex borderTopRight images
    _       -> blank

nextProbability :: StdGen -> Int
nextProbability generator = fst $ randomR (1, 10) generator

nextCoords :: Coordinates -> Coordinates
nextCoords current@(x, y) = case current of   -- If this is returned, STOP recursion!
                                (_, 24) -> (x + 1, 0)   -- each time a row is complete, restart in next column.
                                (_, _) -> (x, y + 1)    -- otherwise, increment the row number.



{- generateFloorDimensions :: StdGen -> Coordinates
generateFloorDimensions generator =
    (fst $ randomR (5, 45) generator, fst $ randomR (5, 45) generator)

getBasicFloorImages :: PictureList -> StdGen -> PictureList
getBasicFloorImages images generator =
    let dimensions = generateFloorDimensions generator
    in  createFloorPictures dimensions images

{- Gives you a list of pictures that you need to make the floor -}
createFloorPictures :: Coordinates -> PictureList -> PictureList
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

createFloorTiles :: PictureList -> TileList
createFloorTiles floorPictures = []
 -}
