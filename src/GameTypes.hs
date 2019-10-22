module GameTypes where

import           Graphics.Gloss

 -- | A data structure to hold the state of the game.
data GameState = Game
  { tiles :: [Tile]
  , effects :: [Effect]
  } deriving Show

data Effect = Effect
  { frames :: [Picture]
    , frameCount :: Integer
  } deriving Show

data Tile = Tile
  { picture :: Picture
  , columnNumber :: Integer
  , rowNumber :: Integer
  } deriving Show

data ImagePath = ImagePath
  { path :: String
    , imageId :: Int
  } deriving Show
