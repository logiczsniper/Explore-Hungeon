module ImageConstants where

import           GameTypes (ImageId)

-- Basic number associations for the various tiles.
-- Each number is that tile's index in the all images list.
borderCorner, borderSide :: ImageId
borderCorner = 0

borderSide = 1

floorDryVertical, floorDryHorizontal :: ImageId
floorDryVertical = 2

floorDryHorizontal = 3

floorDryCornerRight, floorDryCornerLeft :: ImageId
floorDryCornerRight = 4

floorDryCornerLeft = 5

floorDryDoor, floorDryPlain :: ImageId
floorDryDoor = 6

floorDryPlain = 7

floorWetVertical, floorWetHorizontal :: ImageId
floorWetVertical = 8

floorWetHorizontal = 9

floorWetCornerRight, floorWetCornerLeft :: ImageId
floorWetCornerRight = 10

floorWetCornerLeft = 11

floorWetPlain :: ImageId
floorWetPlain = 12

floorPlantsOne, floorPlantsTwo, floorPlantsThree :: ImageId
floorPlantsOne = 13

floorPlantsTwo = 14

floorPlantsThree = 15

wallSecretClosed, wallSecretCracked, wallSecretOpen :: ImageId
wallSecretClosed = 16

wallSecretCracked = 17

wallSecretOpen = 18

wallStandardBoarded, wallStandardPlain, wallStandardDoor, wallStandardFancy ::
     ImageId
wallStandardBoarded = 19

wallStandardDoor = 20

wallStandardFancy = 21

wallStandardPlain = 22

wallPlantsOne, wallPlantsTwo, wallPlantsThree :: ImageId
wallPlantsOne = 23

wallPlantsTwo = 24

wallPlantsThree = 25

floorDryChest :: ImageId
floorDryChest = 26

barrel :: ImageId
barrel = 27

pillar :: ImageId
pillar = 28

waterBarrel :: ImageId
waterBarrel = 29
