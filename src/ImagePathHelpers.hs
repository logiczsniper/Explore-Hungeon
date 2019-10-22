module ImagePathHelpers
  ( borderPath
  , floorPath
  , wallPath
  )
where

newPath :: String -> String -> String
newPath subDirectory name =
  "C:/Users/lczer/Desktop/Projects/HaskellGame/Explore/app/assets/images/"
    ++ subDirectory
    ++ "/"
    ++ name
    ++ ".bmp"

borderPath :: String -> String
borderPath = newPath "borders"

floorPath :: String -> String
floorPath = newPath "floors"

wallPath :: String -> String
wallPath = newPath "walls"
