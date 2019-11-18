module Main where

import           Attributes
import           EventHandling
import           ImageFunctions
import           Rendering
import           SoundFunctions
import           Updating

import           Graphics.Gloss.Interface.Pure.Game
import           Sound.ProteaAudio
import           System.Random

main :: IO ()
main
  -- Complete all IO actions.
 = do
  result <- initAudio 64 44100 1024
  backgroundMusic <- getBackgroundMusic
  addons <- getMisc
  frames <- getFrames
  images <- allImages
  generator <- getStdGen
  -- Play background music.
  soundLoop backgroundMusic 1 1 0 1
  -- Play game.
  play
    window
    backgroundColour
    fps
    (initialState (images ++ addons) frames $ randomRs (0, 99) generator)
    render
    (handleEvent (images ++ addons) $ randomRs (0, 99) generator)
    update
