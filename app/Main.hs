module Main where

import           Attributes
import           EventHandling
import           ImageFunctions
import           Rendering
import           Updating

import           Graphics.Gloss.Interface.Pure.Game
import           Sound.ProteaAudio
import           System.Random

main :: IO ()
main
  -- Complete all IO actions.
 = do
  addons <- getMisc
  frames <- getFrames
  images <- allImages
  generator <- getStdGen
  -- Play game.
  play
    window
    backgroundColour
    fps
    (initialState (images ++ addons) frames $ randomRs (0, 99) generator)
    render
    (handleEvent (images ++ addons) $ randomRs (0, 99) generator)
    update
