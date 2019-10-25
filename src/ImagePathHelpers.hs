module ImagePathHelpers
  ( borderPath
  , floorPath
  , wallPath
  )
where

import GameTypes (ImagePath)

newPath :: ImagePath -> ImagePath -> ImagePath
newPath subDirectory name =
  "C:/Users/lczer/Desktop/Projects/HaskellGame/Explore/app/assets/images/"
    ++ subDirectory
    ++ "/"
    ++ name
    ++ ".bmp"

borderPath :: ImagePath -> ImagePath
borderPath = newPath "borders"

floorPath :: ImagePath -> ImagePath
floorPath = newPath "floors"

wallPath :: ImagePath -> ImagePath
wallPath = newPath "walls"
