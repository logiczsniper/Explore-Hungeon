module ImageFunctions
  ( allImages
  , getImage
  , getFrames
  ) where

import           GameTypes
import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import           ImageConstants
import           ImagePathHelpers

imagePaths :: [ImagePath]
imagePaths =
  [ borderPath "corner"
  , borderPath "side"
  , floorPath "dry/verticalSide"
  , floorPath "dry/horizontalSide"
  , floorPath "dry/cornerRight"
  , floorPath "dry/cornerLeft"
  , floorPath "dry/door"
  , floorPath "dry/plain"
  , floorPath "wet/verticalSide"
  , floorPath "wet/horizontalSide"
  , floorPath "wet/cornerRight"
  , floorPath "wet/cornerLeft"
  , floorPath "wet/plain"
  , floorPath "plants/one"
  , floorPath "plants/two"
  , floorPath "plants/three"
  , wallPath "secret/closed"
  , wallPath "secret/cracked"
  , wallPath "secret/open"
  , wallPath "standard/boarded"
  , wallPath "standard/door"
  , wallPath "standard/fancy"
  , wallPath "standard/plain"
  , wallPath "plants/one"
  , wallPath "plants/two"
  , wallPath "plants/three"
  , pointerPath "one"
  , pointerPath "two"
  , pointerPath "three"
  , pointerPath "four"
  , pointerPath "five"
  , pointerPath "six"
  ]

allImages :: IO PictureList
allImages = mapM loadBMP $ take 26 imagePaths

getImage :: Int -> PictureList -> Degrees -> Picture
getImage index images rotation = rotate rotation (images !! (index))

getAlphaImage :: ImagePath -> IO Picture
getAlphaImage path = do
  mPointerImage <- loadJuicyPNG $ path
  case mPointerImage of
    Nothing           -> return blank
    Just pointerImage -> return pointerImage

getFrames :: IO PictureList
getFrames =
  let paths = drop 26 imagePaths
   in mapM getAlphaImage paths
