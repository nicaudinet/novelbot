{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Brain (initBrain, steer)
import Control.Monad (forM_)
import GHC.TypeLits ()
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen hiding (Position)
import Room (RoomDims (..), collide, simpleRoom)
import System.Random (randomRIO)
import Types (Object, ObjectState (..), Simulation)

----------
-- INIT --
----------

width, height :: Int
width = 800
height = 800

ww :: GLdouble -- window width
ww = fromIntegral width

wh :: GLdouble -- window height
wh = fromIntegral height

roomDims :: RoomDims
roomDims =
  RoomDims
    { roomWidth = 600,
      roomHeight = 150,
      thickness = 5,
      doorWidth = 50,
      wallColor = (1, 1, 1)
    }

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
      (RobotState Nothing initBrain) -- Object Attributes

createRobots :: IO [Object]
createRobots = mapM createRobot [1]

---------------
-- GAME LOOP --
---------------

reactToCollision :: Object -> Simulation ()
reactToCollision obj = do
  attribute <- getObjectAttribute obj
  case attribute of
    WallState _ -> pure ()
    RobotState Nothing _ -> pure ()
    RobotState (Just n) brain -> do
      setObjectAttribute (RobotState (Just (n + 1)) brain) obj
      if n < 30
        then do
          (sx, sy) <- getObjectSize obj
          replaceObject obj (updateObjectSize (sx + 1, sy + 1))
        else do
          setObjectCurrentPicture 0 obj
          setObjectAsleep True obj

gameCycle :: Simulation ()
gameCycle = do
  showFPS TimesRoman24 (ww - 40, 0) 1.0 0.0 0.0
  robots <- getObjectsFromGroup "robotGroup"
  forM_ robots $ \robot -> do
    -- Collisions
    attribute <- getObjectAttribute robot
    case attribute of
      WallState _ -> pure ()
      RobotState Nothing _ -> do
        collide robot
        reactToCollision robot
      RobotState (Just _) _ -> pure ()
    -- Update robots
    steer robot

------------
-- Images --
------------

invisMagenta :: [FilePath] -> [(FilePath, InvList)]
invisMagenta = map (,Just [(255, 0, 255)])

loadImages :: [String] -> [(FilePath, InvList)]
loadImages = invisMagenta . map ("images/" <>)

----------
-- Main --
----------

main :: IO ()
main = do
  let winConfig = ((50, 50), (width, height), "Hello world")
  let gameMap = colorMap 0.0 0.0 0.0 250 250
  let walls = objectGroup "roomGroup" (simpleRoom (ww, wh) roomDims)
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
