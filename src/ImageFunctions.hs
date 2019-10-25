module ImageFunctions
    ( allImages
    , getPictureFromIndex
    )
where

import           GameTypes
import           ImagePathHelpers
import           Graphics.Gloss

imagePaths :: [ImageHolder]
imagePaths =
    [ buildImageHolder (borderPath "bottom")         1
    , buildImageHolder (borderPath "top")            2
    , buildImageHolder (borderPath "right")          3
    , buildImageHolder (borderPath "left")           4
    , buildImageHolder (borderPath "topLeft")        5
    , buildImageHolder (borderPath "topRight")       6
    , buildImageHolder (borderPath "bottomRight")    7
    , buildImageHolder (borderPath "bottomLeft")     9
    , buildImageHolder (floorPath "dry/bottom")      10
    , buildImageHolder (floorPath "dry/top")         11
    , buildImageHolder (floorPath "dry/right")       12
    , buildImageHolder (floorPath "dry/left")        13
    , buildImageHolder (floorPath "dry/topLeft")     14
    , buildImageHolder (floorPath "dry/topRight")    15
    , buildImageHolder (floorPath "dry/bottomRight") 16
    , buildImageHolder (floorPath "dry/bottomLeft")  17
    , buildImageHolder (floorPath "dry/door")        18
    , buildImageHolder (floorPath "dry/plain")       19
    , buildImageHolder (floorPath "wet/bottom")      20
    , buildImageHolder (floorPath "wet/top")         21
    , buildImageHolder (floorPath "wet/right")       22
    , buildImageHolder (floorPath "wet/left")        23
    , buildImageHolder (floorPath "wet/topLeft")     24
    , buildImageHolder (floorPath "wet/topRight")    25
    , buildImageHolder (floorPath "wet/bottomRight") 26
    , buildImageHolder (floorPath "wet/bottomLeft")  27
    , buildImageHolder (floorPath "wet/plain")       28
    , buildImageHolder (floorPath "plants/one")      29
    , buildImageHolder (floorPath "plants/two")      30
    , buildImageHolder (floorPath "plants/three")    31
    , buildImageHolder (wallPath "secret/closed")    32
    , buildImageHolder (wallPath "secret/cracked")   33
    , buildImageHolder (wallPath "secret/open")      34
    , buildImageHolder (wallPath "standard/boarded") 35
    , buildImageHolder (wallPath "standard/door")    36
    , buildImageHolder (wallPath "standard/fancy")   37
    , buildImageHolder (wallPath "standard/plain")   38
    , buildImageHolder (wallPath "plants/one")       39
    , buildImageHolder (wallPath "plants/two")       40
    , buildImageHolder (wallPath "plants/three")     41
    ]

allImages :: IO PictureList
allImages = mapM loadBMP $ map path imagePaths

getPictureFromIndex :: Int -> PictureList -> Picture
getPictureFromIndex index images = images !! index

buildImageHolder :: ImagePath -> ImageId -> ImageHolder
buildImageHolder newPath newId = ImageHolder { path = newPath, imageId = newId }
