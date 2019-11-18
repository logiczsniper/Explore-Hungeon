module ImageFunctions
  ( allImages
  , getImage
  , getFrames
  , getMisc
  ) where

import           GameTypes
import           Graphics.Gloss
import           Graphics.Gloss.Juicy
import           ImageConstants
import           ImagePathHelpers

-- All file paths to images.
imageBmpPaths :: [ImagePath]
imageBmpPaths =
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
  , floorPath "dry/chest"
  ]

imagePngPaths :: [ImagePath]
imagePngPaths =
  [ pointerPath "one"
  , pointerPath "two"
  , pointerPath "three"
  , pointerPath "four"
  , pointerPath "five"
  , pointerPath "six"
  , miscPath "barrel"
  , miscPath "pillar"
  , miscPath "waterBarrel"
  ]

-- Get all TILE images.
allImages :: IO PictureList
allImages = mapM loadBMP imageBmpPaths

-- Get an image from its image constant value and apply it's rotation.
getImage :: Int -> PictureList -> Degrees -> Picture
getImage index images rotation = rotate rotation (images !! (index))

-- Get an image using Juicy Pixels which contains alpha channel (png format).
getAlphaImage :: ImagePath -> IO Picture
getAlphaImage path = do
  mPointerImage <- loadJuicyPNG $ path
  case mPointerImage of
    Nothing           -> return blank
    Just pointerImage -> return pointerImage

-- Get all FRAME images (for the pointer only).
getFrames :: IO PictureList
getFrames =
  let paths = take 6 imagePngPaths
   in mapM getAlphaImage paths

-- Get addons - larger objects rendered above a tile. Not 16 by 16.
getMisc :: IO PictureList
getMisc =
  let paths = drop 6 imagePngPaths
   in mapM getAlphaImage paths
