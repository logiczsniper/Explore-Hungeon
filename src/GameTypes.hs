module GameTypes where

import           Graphics.Gloss
 -- | A data structure to hold the state of the game.

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

data ImageHolder =
  ImageHolder
    { path    :: ImagePath
    , imageId :: ImageId
    }
  deriving (Show)

type Coordinates = (Int, Int)

type TileList = [Tile]

type PictureList = [Picture]

type Width = Int

type Length = Int

type ImagePath = String

type ImageId = Int
