{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Types
  ( -- Basic types
    Color,
    -- Wall Types
    WallBound (..),
    -- Brain Types
    Direction (..),
    CardinalVector (..),
    Distance (..),
    SensoryInput (..),
    Brain (..),
    -- Object types
    Object,
    ObjectState (..),
    Simulation,
  )
where

import Graphics.Rendering.OpenGL (GLdouble, GLfloat)
import Graphics.UI.Fungen (GameObject, IOGame)
import Graphics.UI.Fungen.Types (Point2D)
import qualified Numeric.LinearAlgebra.Static as LA

-------------------
-- Generic Types --
-------------------

type Color = (GLfloat, GLfloat, GLfloat)

----------------
-- Wall Types --
----------------

data WallBound where
  WallBound ::
    { top :: GLdouble,
      bottom :: GLdouble,
      right :: GLdouble,
      left :: GLdouble
    } ->
    WallBound
  deriving (Show)

-----------------
-- Brain Types --
-----------------

data Direction = North | South | East | West

data CardinalVector where
  CardinalVector :: Point2D -> Direction -> CardinalVector

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

newtype Brain = Brain {unBrain :: LA.L 6 2}

------------------
-- Object Types --
------------------

data ObjectState where
  RobotState :: {timeSinceBoom :: Maybe Int, robotBrain :: Brain} -> ObjectState
  WallState :: {bound :: WallBound} -> ObjectState

type Object = GameObject ObjectState

type Simulation = IOGame () ObjectState () ()
