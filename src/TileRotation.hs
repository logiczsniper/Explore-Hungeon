module TileRotation where

import           GameTypes
import           Graphics.Gloss

tileRotate :: Tile -> Tile
tileRotate tile =
  Tile
    { picture = rotate (rotation tile) $ picture tile
    , columnNumber = columnNumber tile
    , rowNumber = rowNumber tile
    , rotation = rotation tile
    }
