{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Room
  ( RoomDims (..),
    simpleRoom,
    collide,
  )
where

import GHC.TypeLits ()
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen hiding (Position)
import Types (Color, Object, ObjectState (..), Simulation)
import Wall (rectangleWall)

data RoomDims where
  RoomDims ::
    { roomWidth :: GLdouble,
      roomHeight :: GLdouble,
      thickness :: GLdouble,
      doorWidth :: GLdouble,
      wallColor :: Color
    } ->
    RoomDims

simpleRoom :: Point2D -> RoomDims -> [Object]
simpleRoom (w, h) RoomDims {..} = [wall1, wall2, wall3, wall4, wall5]
  where
    wall1 :: Object
    wall1 =
      let dimentions = (thickness, roomHeight + thickness)
          position = (w / 2 - roomWidth / 2, h / 2)
       in rectangleWall 1 dimentions position wallColor

    wall2 :: Object
    wall2 =
      let dimentions = (roomWidth, thickness)
          position = (w / 2, h / 2 - roomHeight / 2)
       in rectangleWall 2 dimentions position wallColor

    wall3 :: Object
    wall3 =
      let dimentions = (thickness, roomHeight + thickness)
          position = (w / 2 + roomWidth / 2, h / 2)
       in rectangleWall 3 dimentions position wallColor

    wall4 :: Object
    wall4 =
      let dimentions = (30, thickness)
          position = (w / 2 + roomWidth / 2 - 15, h / 2 + roomHeight / 2)
       in rectangleWall 4 dimentions position wallColor

    wall5 :: Object
    wall5 =
      let dimentions = (roomWidth - 30 - doorWidth, thickness)
          position = (w / 2 - roomWidth / 2 + fst dimentions / 2, h / 2 + roomHeight / 2)
       in rectangleWall 5 dimentions position wallColor

explode :: Object -> Simulation ()
explode obj = do
  replaceObject obj (updateObjectSize (100, 100))
  setObjectCurrentPicture 2 obj
  setObjectSpeed (0, 0) obj
  attribute <- getObjectAttribute obj
  case attribute of
    WallState _ -> pure ()
    RobotState _ -> setObjectAttribute (RobotState (Just 0)) obj

collide :: Object -> Simulation ()
collide robot = do
  -- Vertical wall collisions
  wall1 <- findObject "wall-1" "roomGroup"
  wall3 <- findObject "wall-3" "roomGroup"
  vColl <- objectListObjectCollision [wall1, wall3] robot
  when vColl (explode robot)
  -- Horizontal wall collisions
  wall2 <- findObject "wall-2" "roomGroup"
  wall4 <- findObject "wall-4" "roomGroup"
  wall5 <- findObject "wall-5" "roomGroup"
  hColl <- objectListObjectCollision [wall2, wall4, wall5] robot
  when hColl (reverseYSpeed robot)
