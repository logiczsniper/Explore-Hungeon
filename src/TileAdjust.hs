module TileAdjust
  ( tileAdjust
  , calculateTranslation
  ) where

import           GameTypes
import           Graphics.Gloss
import           Numeric.Extra

-- Calculates the linear translation required for the given value.
-- Value is a rowNumber or a columnNumber.
calculateTranslation :: Int -> Float
calculateTranslation number = intToFloat $ 16 * (number + 1) - 416

-- Translates the picture of the tile to its correct position in the grid.
-- Also moves every picture 200 200, (roughly) centering the map in the window.
tileAdjust :: Tile -> Tile
tileAdjust tile =
  Tile
    { picture =
        translate 200 200 $
        translate
          (calculateTranslation $ columnNumber tile)
          (calculateTranslation $ rowNumber tile) $
        picture tile
    , columnNumber = columnNumber tile
    , rowNumber = rowNumber tile
    , isEntrance = isEntrance tile
    , isBorder = isBorder tile
    }
