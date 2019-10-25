module ImageFunctions
    ( allImages
    , getPictureFromIndex
    )
where

import           GameTypes
import           ImagePathHelpers
import           Graphics.Gloss

imagePaths :: [ImagePath]
imagePaths =
    [ buildImagePath (borderPath "bottom")         1
    , buildImagePath (borderPath "top")            2
    , buildImagePath (borderPath "right")          3
    , buildImagePath (borderPath "left")           4
    , buildImagePath (borderPath "topLeft")        5
    , buildImagePath (borderPath "topRight")       6
    , buildImagePath (borderPath "bottomRight")    7
    , buildImagePath (borderPath "bottomLeft")     9
    , buildImagePath (floorPath "dry/bottom")      10
    , buildImagePath (floorPath "dry/top")         11
    , buildImagePath (floorPath "dry/right")       12
    , buildImagePath (floorPath "dry/left")        13
    , buildImagePath (floorPath "dry/topLeft")     14
    , buildImagePath (floorPath "dry/topRight")    15
    , buildImagePath (floorPath "dry/bottomRight") 16
    , buildImagePath (floorPath "dry/bottomLeft")  17
    , buildImagePath (floorPath "dry/door")        18
    , buildImagePath (floorPath "dry/plain")       19
    , buildImagePath (floorPath "wet/bottom")      20
    , buildImagePath (floorPath "wet/top")         21
    , buildImagePath (floorPath "wet/right")       22
    , buildImagePath (floorPath "wet/left")        23
    , buildImagePath (floorPath "wet/topLeft")     24
    , buildImagePath (floorPath "wet/topRight")    25
    , buildImagePath (floorPath "wet/bottomRight") 26
    , buildImagePath (floorPath "wet/bottomLeft")  27
    , buildImagePath (floorPath "wet/plain")       28
    , buildImagePath (floorPath "plants/one")      29
    , buildImagePath (floorPath "plants/two")      30
    , buildImagePath (floorPath "plants/three")    31
    , buildImagePath (wallPath "secret/closed")    32
    , buildImagePath (wallPath "secret/cracked")   33
    , buildImagePath (wallPath "secret/open")      34
    , buildImagePath (wallPath "standard/boarded") 35
    , buildImagePath (wallPath "standard/door")    36
    , buildImagePath (wallPath "standard/fancy")   37
    , buildImagePath (wallPath "standard/plain")   38
    , buildImagePath (wallPath "plants/one")       39
    , buildImagePath (wallPath "plants/two")       40
    , buildImagePath (wallPath "plants/three")     41
    ]

allImages :: IO PictureList
allImages = mapM loadBMP $ map path imagePaths

getPictureFromIndex :: Int -> PictureList -> Picture
getPictureFromIndex index images = images !! index

buildImagePath :: String -> Int -> ImagePath
buildImagePath newPath newId = ImagePath { path = newPath, imageId = newId }
