module SoundFunctions where

import           Control.Monad
import           GameTypes
import           Sound.ProteaAudio

getBackgroundMusic :: IO Sample
getBackgroundMusic =
  sampleFromFile
    "C:/Users/lczer/Desktop/Projects/HaskellGame/Explore/app/assets/samples/background.wav"
    1.0
