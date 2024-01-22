{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Types
  ( -- Basic types
    Color,
    -- Wall Types
    Line (..),
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
  PosInfinite :: Distance
  NegInfinite :: Distance
  Finite :: Double -> Distance
  deriving (Show)

data Line where
  Line :: {from :: (Double, Double), to :: (Double, Double)} -> Line
  deriving (Show)

data SensoryInput where
  SensoryInput ::
    { north :: Maybe Double,
      south :: Maybe Double,
      east :: Maybe Double,
      est :: Maybe Double,
      speed :: Point2D
    } ->
    SensoryInput
  deriving (Show)

newtype Brain = Brain {unBrain :: LA.L 6 2}

------------------
-- Object Types --
------------------

data ObjectState where
  WallState :: {bound :: WallBound} -> ObjectState
  RobotState ::
    { timeSinceStart :: Int,
      timeSinceBoom :: Maybe Int,
      robotBrain :: Brain,
      prevPos :: Point2D
    } ->
    ObjectState

type Object = GameObject ObjectState

type Simulation = IOGame () ObjectState () ()
