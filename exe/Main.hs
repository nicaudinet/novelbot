{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (forM_)
import GHC.TypeLits ()
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen hiding (Position)
import Robot (createRobot, stepRobot)
import Room (RoomDims (..), collide, simpleRoom)
import Types (ObjectState (..), Simulation)

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

robotIds :: [Int]
robotIds = [1 .. 10]

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
      RobotState {..} ->
        case timeSinceBoom of
          Nothing -> collide robot
          Just _ -> pure ()
    stepRobot robot

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
  robots <- objectGroup "robotGroup" <$> mapM (createRobot (ww, wh)) robotIds
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
    (Timer 200) -- main loop timing (in milliseconds)
    bmpList -- image files
