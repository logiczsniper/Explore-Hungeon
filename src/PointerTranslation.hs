module PointerTranslation
  ( translatePointer
  ) where

import           GameTypes
import           Graphics.Gloss
import           Numeric.Extra

{- The number starts at 0. This gets the nth term in sequence- non zero. -}
translatePointer :: Pointer -> Coordinates -> Pointer
translatePointer pointer (x, y) =
  translate
    (intToFloat $ 16 * (x + 1) - 408)
    (intToFloat $ 16 * (y + 1) - 408)
    pointer
