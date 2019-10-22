module GenerateMap
    ( generateMap
    )
where

import           Graphics.Gloss                 ( Picture )
import           GameTypes                      ( Tile )
import           System.Random

{-
STEPS:

1) Randomly generate two integers between 5 and 45- these are the
    length and width of the floor.
2) From those dimensions, create a picture object from pictures [Picture] where
    each Picture is a tile that has been translated. This is single picture is all the floors.
3) Using the [Tile] of floor tiles, create a [Tile] for the wall tiles.
4) Using the [Tile] of wall tiles, create a [Tile] for the border tiles.
-}
generateMap :: [Picture] -> [Tile]
generateMap images = []

generateFloorDimensions :: StdGen -> (Int, Int)
generateFloorDimensions generator =
    (fst $ randomR (5, 45) generator, fst $ randomR (5, 45) generator)

createFloorPictures :: (Int, Int) -> [Picture]
createFloorPictures dimensions = []

getBasicFloorImages :: [Picture] -> StdGen -> [Picture]
getBasicFloorImages images generator =
    let dimensions = generateFloorDimensions generator
    in  createFloorPictures dimensions

