module ImageFunctions
  ( allImages
  , getPictureFromConstant
  ) where

import           GameTypes
import           Graphics.Gloss
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
  ]

allImages :: IO PictureList
allImages = mapM loadBMP imagePaths

getPictureFromConstant :: Int -> PictureList -> Degrees -> Picture
getPictureFromConstant index images rotation =
  rotate rotation (images !! (index))
