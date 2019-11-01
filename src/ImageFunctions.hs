module ImageFunctions
  ( allImages
  , getPictureFromConstant
  , getRotationFromPicture
  ) where

import           GameTypes
import           Graphics.Gloss
import           ImageConstants
import           ImagePathHelpers

imagePaths :: [ImageHolder]
imagePaths =
  [ buildImageHolder (borderPath "top") 1
  , buildImageHolder (borderPath "top") 2
  , buildImageHolder (borderPath "right") 3
  , buildImageHolder (borderPath "right") 4
  , buildImageHolder (borderPath "bottomRight") 7
  , buildImageHolder (borderPath "bottomRight") 5
  , buildImageHolder (borderPath "bottomLeft") 8
  , buildImageHolder (borderPath "bottomLeft") 6
  -- End of border
  , buildImageHolder (floorPath "dry/top") 9
  , buildImageHolder (floorPath "dry/top") 10
  , buildImageHolder (floorPath "dry/right") 11
  , buildImageHolder (floorPath "dry/right") 12
  , buildImageHolder (floorPath "dry/bottomRight") 14
  , buildImageHolder (floorPath "dry/bottomRight") 15
  , buildImageHolder (floorPath "dry/bottomLeft") 13
  , buildImageHolder (floorPath "dry/bottomLeft") 16
  , buildImageHolder (floorPath "dry/door") 17
  , buildImageHolder (floorPath "dry/plain") 18
  -- End of dry floor
  , buildImageHolder (floorPath "wet/top") 19
  , buildImageHolder (floorPath "wet/top") 20
  , buildImageHolder (floorPath "wet/right") 21
  , buildImageHolder (floorPath "wet/right") 22
  , buildImageHolder (floorPath "wet/bottomRight") 24
  , buildImageHolder (floorPath "wet/bottomRight") 25
  , buildImageHolder (floorPath "wet/bottomLeft") 23
  , buildImageHolder (floorPath "wet/bottomLeft") 26
  , buildImageHolder (floorPath "wet/plain") 27
  -- End of wet floor
  , buildImageHolder (floorPath "plants/one") 28
  , buildImageHolder (floorPath "plants/two") 29
  , buildImageHolder (floorPath "plants/three") 30
  -- End of plant floor
  , buildImageHolder (wallPath "secret/closed") 31
  , buildImageHolder (wallPath "secret/cracked") 32
  , buildImageHolder (wallPath "secret/open") 33
  , buildImageHolder (wallPath "standard/boarded") 34
  , buildImageHolder (wallPath "standard/door") 35
  , buildImageHolder (wallPath "standard/fancy") 36
  , buildImageHolder (wallPath "standard/plain") 37
  , buildImageHolder (wallPath "plants/one") 38
  , buildImageHolder (wallPath "plants/two") 39
  , buildImageHolder (wallPath "plants/three") 40
  -- End of walls
  ]

allImages :: IO PictureList
allImages = mapM loadBMP $ map path imagePaths

{-
Note: the constant value has one subtracted from it
because we need the index whereas the constant value
begins counting at 1, instead of 0.
-}
getPictureFromConstant :: Int -> PictureList -> Picture
getPictureFromConstant constant images = images !! (constant - 1)

getRotationFromPicture :: Picture -> PictureList -> Degrees
getRotationFromPicture picture images
  | elem
     picture
     [ getPictureFromConstant borderTopRight images
     , getPictureFromConstant borderTopLeft images
     , getPictureFromConstant borderLeft images
     , getPictureFromConstant borderBottom images
     , getPictureFromConstant floorDryTopRight images
     , getPictureFromConstant floorDryTopLeft images
     , getPictureFromConstant floorDryLeft images
     , getPictureFromConstant floorDryBottom images
     , getPictureFromConstant floorWetTopRight images
     , getPictureFromConstant floorWetTopLeft images
     , getPictureFromConstant floorWetLeft images
     , getPictureFromConstant floorWetBottom images
     ] = 180
  | otherwise = 0

buildImageHolder :: ImagePath -> ImageId -> ImageHolder
buildImageHolder newPath newId = ImageHolder {path = newPath, imageId = newId}
