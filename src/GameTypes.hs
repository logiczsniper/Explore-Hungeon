module GameTypes where

import           Graphics.Gloss

data GameState =
  GameState
    { tiles           :: [Tile]
    , pointerState    :: PointerState
    , mapNumber       :: MapNumber
    , currentDuration :: Duration
    }
  deriving (Show)

data PointerState =
  PointerState
    { frames :: [Picture]
    , index  :: Int
    , size   :: Size
    , coords :: Coordinates
    }
  deriving (Show)

data Tile =
  Tile
    { picture      :: Picture
    , columnNumber :: Int
    , rowNumber    :: Int
    , isEntrance   :: Bool
    }
  deriving (Show, Eq)

type Coordinates = (Int, Int)

type Dimensions = (Width, Length)

type TileList = [Tile]

type PictureList = [Picture]

type RandomList = [Int]

type Probability = [Int]

type Pointer = Picture

type ImagePath = String

type ImageId = Int

type Width = Int

type Length = Int

type MapNumber = Int

type Degrees = Float

type Size = Float

type Duration = Float

data MapType
  = Dry
  | Wet
  deriving (Enum, Eq)
