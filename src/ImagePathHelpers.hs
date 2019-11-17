module ImagePathHelpers
  ( borderPath
  , floorPath
  , wallPath
  , pointerPath
  , miscPath
  ) where

import           GameTypes (ImagePath)

newPath :: ImagePath -> ImagePath -> ImagePath
newPath subDirectory name =
  "C:/Users/lczer/Desktop/Projects/HaskellGame/Explore/app/assets/images/" ++
  subDirectory ++ "/" ++ name ++ ".bmp"

borderPath :: ImagePath -> ImagePath
borderPath = newPath "borders"

floorPath :: ImagePath -> ImagePath
floorPath = newPath "floors"

wallPath :: ImagePath -> ImagePath
wallPath = newPath "walls"

pointerPath :: ImagePath -> ImagePath
pointerPath name =
  "C:/Users/lczer/Desktop/Projects/HaskellGame/Explore/app/assets/images/pointer/" ++
  name ++ ".png"

miscPath :: ImagePath -> ImagePath
miscPath name =
  "C:/Users/lczer/Desktop/Projects/HaskellGame/Explore/app/assets/images/misc/" ++
  name ++ ".png"
