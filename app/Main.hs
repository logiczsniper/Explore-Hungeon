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

import           Control.Concurrent
import           Control.Monad

main :: IO ()
main
  -- Complete all IO actions.
 = do
  addons <- getMisc
  frames <- getFrames
  images <- allImages
  sounds <- getSounds
  generator <- getStdGen
  soundLoop (sounds !! 0) 1 1 0 1
  -- Play game.
  play
    window
    backgroundColour
    fps
    (initialState (images ++ addons) frames $ randomRs (0, 99) generator)
    render
    (handleEvent (images ++ addons) sounds $ randomRs (0, 99) generator)
    update
