module EventHandling
  ( handleEvent
  ) where

import           GameTypes
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           PointerFunctions

-- Handle the mouse input from the user, changing the game state.
-- Controls: WASD for directions, E to enter a door, Q to quit the game.
handleEvent :: PictureList -> RandomList -> Event -> GameState -> GameState
handleEvent images randomList key initState =
  case key of
    EventKey (Char 'w') Down _ _ -> movePointer initState (x, y + 1)
    EventKey (Char 's') Down _ _ -> movePointer initState (x, y - 1)
    EventKey (Char 'a') Down _ _ -> movePointer initState (x - 1, y)
    EventKey (Char 'd') Down _ _ -> movePointer initState (x + 1, y)
    EventKey (Char 'q') Down _ _ -> error "Player has quit."
    EventKey (Char 'e') Down _ _ ->
      tilePointerInteraction pointerCoords initState images randomList
    otherwise -> initState
  where
    pointerCoords = coords $ pointerState initState
    x = fst pointerCoords
    y = snd pointerCoords
