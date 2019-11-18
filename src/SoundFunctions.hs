module SoundFunctions where

import           Control.Monad
import           GameTypes
import           Sound.ProteaAudio

samplePath :: SoundPath -> SoundPath
samplePath name =
  "C:/Users/lczer/Desktop/Projects/HaskellGame/Explore/app/assets/samples/" ++
  name ++ ".wav"

soundPaths :: [SoundPath]
soundPaths = [samplePath "background", samplePath "enter"]

getSounds :: IO [Sample]
getSounds = mapM sampleFromFile' soundPaths

sampleFromFile' :: SoundPath -> IO Sample
sampleFromFile' path = sampleFromFile path 1.0
