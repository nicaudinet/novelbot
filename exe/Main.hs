{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Maybe (catMaybes)
import Graphics.Rendering.OpenGL (GLdouble, GLfloat)
import Graphics.UI.Fungen hiding (Position)
import System.Random (randomRIO)

type Name = String

type Dimentions = Point2D

type Position = Point2D

type Color = (GLfloat, GLfloat, GLfloat)

data WallBound where
  WallBound ::
    { top :: GLdouble,
      bottom :: GLdouble,
      right :: GLdouble,
      left :: GLdouble
    } ->
    WallBound
  deriving (Show)

data ObjectState where
  RobotState :: {timeSinceBoom :: Maybe Int} -> ObjectState
  WallState :: {bound :: WallBound} -> ObjectState

type Object = GameObject ObjectState

type Simulation = IOGame () ObjectState () ()

data Direction = North | South | East | West

data CardinalVector where
  CardinalVector :: Position -> Direction -> CardinalVector

data Distance where
  Infinite :: Distance
  Finite :: Double -> Distance
  deriving (Show)

data SensoryInput where
  SensoryInput ::
    { north :: Distance,
      south :: Distance,
      east :: Distance,
      west :: Distance,
      speed :: Point2D
    } ->
    SensoryInput
  deriving (Show)

width, height :: Int
width = 800
height = 800

ww :: GLdouble -- window width
ww = fromIntegral width

wh :: GLdouble -- window height
wh = fromIntegral height

invisMagenta :: [FilePath] -> [(FilePath, InvList)]
invisMagenta = map (,Just [(255, 0, 255)])

loadImages :: [String] -> [(FilePath, InvList)]
loadImages = invisMagenta . map ("images/" <>)

main :: IO ()
main = do
  let winConfig = ((50, 50), (width, height), "Hello world")
  let gameMap = colorMap 0.0 0.0 0.0 250 250
  let walls = objectGroup "roomGroup" createWalls
  robots <- objectGroup "robotGroup" <$> createRobots
  let bindings = [(Char 'q', Press, \_ _ -> funExit)]
  let bmpList = loadImages ["empty.bmp", "robot.bmp", "boom.bmp"]
  funInit
    winConfig -- main window layout
    gameMap -- background
    [walls, robots] -- object groups
    () -- initial game state
    () -- initial game attribute
    bindings -- input bindings
    gameCycle -- step action
    (Timer 30) -- main loop timing (in milliseconds)
    bmpList -- image files

----------
-- INIT --
---------

createRobot :: Int -> IO Object
createRobot index = do
  speed <- (,) <$> randomRIO (-5, 5) <*> randomRIO (-5, 5)
  pure $
    object
      ("robot-" <> show index) -- name
      (Tex (25, 25) 1) -- object picture
      False -- asleep
      (ww / 2, wh / 2) -- position
      speed -- speed
      (RobotState Nothing) -- Object Attributes

createRobots :: IO [Object]
createRobots = mapM createRobot [1]

rectangleBound :: GLdouble -> GLdouble -> WallBound
rectangleBound roomWidth roomHeight =
  WallBound
    { top = roomHeight / 2,
      bottom = -roomHeight / 2,
      right = roomWidth / 2,
      left = -roomWidth / 2
    }

boundToList :: WallBound -> [Point2D]
boundToList WallBound {..} =
  [ (right, top),
    (left, top),
    (left, bottom),
    (right, bottom)
  ] -- counter-clockwise order

createWall :: Int -> Dimentions -> Position -> Color -> Object
createWall index dims pos color =
  let bound = uncurry rectangleBound dims
      (r, g, b) = color
      picture = Basic (Polyg (boundToList bound) r g b Filled)
   in object ("wall-" <> show index) picture False pos (0, 0) (WallState bound)

createWalls :: [Object]
createWalls = [wall1, wall2, wall3, wall4, wall5]
  where
    roomWidth :: GLdouble
    roomWidth = 600

    roomHeight :: GLdouble
    roomHeight = 150

    thickness :: GLdouble
    thickness = 5

    doorWidth :: GLdouble
    doorWidth = 50

    color :: Color
    color = (1.0, 1.0, 1.0) -- white
    wall1 :: Object
    wall1 =
      let dimentions = (thickness, roomHeight + thickness)
          position = (ww / 2 - roomWidth / 2, wh / 2)
       in createWall 1 dimentions position color

    wall2 :: Object
    wall2 =
      let dimentions = (roomWidth, thickness)
          position = (ww / 2, wh / 2 - roomHeight / 2)
       in createWall 2 dimentions position color

    wall3 :: Object
    wall3 =
      let dimentions = (thickness, roomHeight + thickness)
          position = (ww / 2 + roomWidth / 2, wh / 2)
       in createWall 3 dimentions position color

    wall4 :: Object
    wall4 =
      let dimentions = (30, thickness)
          position = (ww / 2 + roomWidth / 2 - 15, wh / 2 + roomHeight / 2)
       in createWall 4 dimentions position color

    wall5 :: Object
    wall5 =
      let dimentions = (roomWidth - 30 - doorWidth, thickness)
          position = (ww / 2 - roomWidth / 2 + fst dimentions / 2, wh / 2 + roomHeight / 2)
       in createWall 5 dimentions position color

-----------
-- BRAIN --
-----------

distanceToWall :: CardinalVector -> Position -> WallBound -> Maybe GLdouble
distanceToWall (CardinalVector (x, y) direction) wallPos wallBound =
  let t = snd wallPos + top wallBound
      b = snd wallPos + bottom wallBound
      r = fst wallPos + right wallBound
      l = fst wallPos + left wallBound
   in case direction of
        North ->
          if y < b && l < x && x < r
            then Just (b - y)
            else Nothing
        South ->
          if y > t && l < x && x < r
            then Just (y - t)
            else Nothing
        East ->
          if x < l && b < y && y < t
            then Just (l - x)
            else Nothing
        West ->
          if x > r && b < y && y < t
            then Just (x - r)
            else Nothing

minDistance :: [Double] -> Distance
minDistance [] = Infinite
minDistance (x : xs) = Finite (foldr min x xs)

distanceToWalls :: CardinalVector -> Simulation Distance
distanceToWalls vec = do
  walls <- getObjectsFromGroup "roomGroup"
  distances <- forM walls $ \wall -> do
    wallPos <- getObjectPosition wall
    WallState wallBound <- getObjectAttribute wall
    pure (distanceToWall vec wallPos wallBound)
  pure (minDistance (catMaybes distances))

sense :: Object -> Simulation SensoryInput
sense obj = do
  position <- getObjectPosition obj
  SensoryInput
    <$> distanceToWalls (CardinalVector position North)
    <*> distanceToWalls (CardinalVector position South)
    <*> distanceToWalls (CardinalVector position East)
    <*> distanceToWalls (CardinalVector position West)
    <*> getObjectSpeed obj

-- constant speed
think :: SensoryInput -> Point2D
think (SensoryInput n s e w (x, y))
  | x <= 0 && w `lessThan` dist = (strength, 0)
  | x > 0 && e `lessThan` dist = (-strength, 0)
  | y <= 0 && s `lessThan` dist = (0, strength)
  | y > 0 && n `lessThan` dist = (0, -strength)
  | otherwise = (0, 0)
  where
    dist, strength :: Double
    dist = 30
    strength = 1

    lessThan :: Distance -> Double -> Bool
    lessThan Infinite _ = True
    lessThan (Finite a) b = a < b

act :: Point2D -> Object -> Simulation ()
act (dx, dy) obj = do
  speed <- getObjectSpeed obj
  let newSpeed = (fst speed + dx, snd speed + dy)
  setObjectSpeed newSpeed obj

-------------
-- UPDATES --
-------------

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

updateRobot :: Object -> Simulation ()
updateRobot obj = do
  -- SENSE -> THINK -> ACT loop
  sensoryInput <- sense obj
  act (think sensoryInput) obj
  -- Reaction to collision
  attribute <- getObjectAttribute obj
  case attribute of
    WallState _ -> pure ()
    RobotState Nothing -> pure ()
    RobotState (Just n) -> do
      setObjectAttribute (RobotState (Just (n + 1))) obj
      if n < 30
        then do
          (sx, sy) <- getObjectSize obj
          replaceObject obj (updateObjectSize (sx + 1, sy + 1))
        else do
          setObjectCurrentPicture 0 obj
          setObjectAsleep True obj

---------------
-- GAME LOOP --
---------------

gameCycle :: Simulation ()
gameCycle = do
  showFPS TimesRoman24 (ww - 40, 0) 1.0 0.0 0.0
  robots <- getObjectsFromGroup "robotGroup"
  forM_ robots $ \robot -> do
    -- Collisions
    attribute <- getObjectAttribute robot
    case attribute of
      WallState _ -> pure ()
      RobotState Nothing -> collide robot
      RobotState (Just _) -> pure ()
    -- Update robots
    updateRobot robot
