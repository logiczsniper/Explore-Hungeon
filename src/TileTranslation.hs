module TileTranslation
    ( tileTranslate
    )
where

import           GameTypes
import           Graphics.Gloss

{- The number starts at 0. This gets the nth term in sequence- non zero. -}
calculateTranslation :: Integer -> Float
calculateTranslation number = fromInteger $ 16 * (number + 1) - 416

tileTranslate :: Tile -> Picture
tileTranslate tile =
    translate (calculateTranslation $ columnNumber tile)
              (calculateTranslation $ rowNumber tile)
        $ picture tile
