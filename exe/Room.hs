{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Room
  ( RoomDims (..),
    simpleRoom,
    collide,
  )
where

import Data.Maybe (catMaybes)
import GHC.TypeLits ()
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen hiding (Position)
import Robot (explode)
import Types (Color, Line (..), Object, ObjectState (..), Simulation)
import Wall (intersectRectangleWall, rectangleWall)

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

collideWithWall :: Object -> Object -> Simulation (Maybe Point2D)
collideWithWall robot wall = do
  currPos <- getObjectPosition robot
  robotAttribute <- getObjectAttribute robot
  case robotAttribute of
    WallState _ -> pure Nothing
    RobotState {..} -> do
      wallAttribute <- getObjectAttribute wall
      case wallAttribute of
        RobotState {} -> pure Nothing
        WallState bound -> do
          wallPos <- getObjectPosition wall
          case intersectRectangleWall (Line prevPos currPos) wallPos bound of
            [] -> pure Nothing
            points ->
              let meanX = sum (map fst points) / fromIntegral (length points)
                  meanY = sum (map snd points) / fromIntegral (length points)
               in pure (Just (meanX, meanY))

collideWithWalls :: Object -> [Object] -> Simulation [Point2D]
collideWithWalls robot walls = catMaybes <$> mapM (collideWithWall robot) walls

distance :: Point2D -> Point2D -> Double
distance (x1, y1) (x2, y2) =
  sqrt ((x1 - x2) ^ (2 :: Integer) + (y1 - y2) ^ (2 :: Integer))

closestTo :: Point2D -> [Point2D] -> Maybe Point2D
closestTo _ [] = Nothing
closestTo p (x : xs) =
  case closestTo p xs of
    Nothing -> Just x
    Just y -> if distance p x < distance p y then Just x else Just y

collide :: Object -> Simulation ()
collide robot = do
  walls <- getObjectsFromGroup "roomGroup"
  collisions <- collideWithWalls robot walls
  pos <- getObjectPosition robot
  case closestTo pos collisions of
    Nothing -> pure ()
    Just p -> do
      explode p robot
