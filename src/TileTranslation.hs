module TileTranslation
    ( tileTranslate
    )
where

import           GameTypes
import           Graphics.Gloss
import           Numeric.Extra

{- The number starts at 0. This gets the nth term in sequence- non zero. -}
calculateTranslation :: Int -> Float
calculateTranslation number = intToFloat $ 16 * (number + 1) - 416

tileTranslate :: Tile -> Picture
tileTranslate tile =
    translate (calculateTranslation $ columnNumber tile)
              (calculateTranslation $ rowNumber tile)
        $ picture tile
