module ImagePathHelpers
  ( borderPath
  , floorPath
  , wallPath
  )
where

generatePath :: String -> String -> String
generatePath subDirectory name =
  "C:/Users/lczer/Desktop/Projects/HaskellGame/Explore/app/assets/images/"
    ++ subDirectory
    ++ "/"
    ++ name
    ++ ".bmp"

borderPath :: String -> String
borderPath = generatePath "borders"

floorPath :: String -> String
floorPath = generatePath "floors"

wallPath :: String -> String
wallPath = generatePath "walls"
