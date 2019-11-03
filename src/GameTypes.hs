module GameTypes where

import           Graphics.Gloss

data GameState =
  Game
    { tiles   :: [Tile]
    , effects :: [Effect]
    }
  deriving (Show)

data Effect =
  Effect
    { frames     :: [Picture]
    , frameCount :: Int
    }
  deriving (Show)

data Tile =
  Tile
    { picture      :: Picture
    , columnNumber :: Int
    , rowNumber    :: Int
    }
  deriving (Show)

type Coordinates = (Int, Int)

type Dimensions = (Width, Length)

type TileList = [Tile]

type PictureList = [Picture]

type RandomList = [Int]

type ImagePath = String

type ImageId = Int

type Width = Int

type Length = Int

type Degrees = Float

data FloorType
  = Dry
  | Wet
  deriving (Enum, Eq)

data WallType
  = Plants
  | Secret
  | Standard
  deriving (Enum, Eq)

data SecretWallType
  = Open
  | Cracked
  | Closed
  deriving (Enum, Eq)

data PlantType
  = One
  | Two
  | Three
  deriving (Enum, Eq)

data StandardWallType
  = Door
  | Boarded
  | Fancy
  | Plain
  deriving (Enum, Eq)
