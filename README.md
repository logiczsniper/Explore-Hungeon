### Explore
Haskell game where you explore the Hungeon (Haskell Dungeon) - a randomly generated dungeon.

### Gameplay
The player controls a flame sprite (WASD keys to move) which is animated (it flickers). The flame gradually shrinks in size as the screen gets darker proportionally. Reach an entrance and press E before your flame simmers out and darkness falls! Or just press Q to quit. <br><br>
When the player reaches a door and enters, the flame is restored and darkness is lifted- however, the further into the game the player progresses the weaker your flame gets, giving you less and less time to reach the next entrance. As the maps are randomly generated, some may not have a door at all. If this is the case, press E for a free pass to the next randomly generated map. <br><br>
While you venture further into the Hungeon, aptly themed music plays in the background. However, much to my own disappointment, I could not figure out how to play sound effects! Playing a Sample is an IO action and I would have to play (for example an 'enter door' effect) the sample inside the handleEvent function, which is pure! As such, you can only enjoy the background music due to the fact that I can load and begin looping the sample in the dirty main function.

### Sources
- [Tile set](https://oddpotatogift.itch.io/16x16-dungeon-tileset)
- [Flame sprite sheet](https://asymmetric.itch.io/mideval-2d-16x16-torch-sprite-pack-with-animations)
- [Background music](www.youtube.com/watch?v=64R-10xNfmk) 
- [Sound effect](https://freesound.org/people/LittleRobotSoundFactory/sounds/270316/) 

### Modules Used
- Graphics.Gloss
- Sound.ProtProteaAudio
- System.Random

### Tools Used
- VScode 
- VIM
- GIMP

### Running
Download the file `Explore-exe.exe` and run!