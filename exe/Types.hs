{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Types
  ( Color,
    WallBound (..),
    Object,
    ObjectState (..),
    Simulation,
  )
where

import Graphics.Rendering.OpenGL (GLdouble, GLfloat)
import Graphics.UI.Fungen (GameObject, IOGame)

data WallBound where
  WallBound ::
    { top :: GLdouble,
      bottom :: GLdouble,
      right :: GLdouble,
      left :: GLdouble
    } ->
    WallBound
  deriving (Show)

type Color = (GLfloat, GLfloat, GLfloat)

data ObjectState where
  RobotState :: {timeSinceBoom :: Maybe Int} -> ObjectState
  WallState :: {bound :: WallBound} -> ObjectState

type Object = GameObject ObjectState

type Simulation = IOGame () ObjectState () ()
