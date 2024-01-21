{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Robot
  ( createRobot,
    stepRobot,
  )
where

import Control.Monad (forM)
import Data.Maybe (catMaybes)
import GHC.TypeLits ()
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen
import qualified Numeric.LinearAlgebra.Static as LA
  ( L,
    headTail,
    matrix,
    unrow,
    (<>),
  )
import System.Random (randomRIO)
import Types
  ( Brain (..),
    CardinalVector (..),
    Direction (..),
    Distance (..),
    Object,
    ObjectState (..),
    SensoryInput (..),
    Simulation,
    WallBound (..),
  )

----------------
-- Robot init --
----------------

initBrain :: Brain
initBrain = Brain $ LA.matrix (replicate 12 0.01)

createRobot :: Point2D -> Int -> IO Object
createRobot (ww, wh) index = do
  robotSpeed <- (,) <$> randomRIO (-5, 5) <*> randomRIO (-5, 5)
  let position = (ww / 2, wh / 2)
  pure $
    object
      ("robot-" <> show index) -- name
      (Tex (25, 25) 1) -- object picture
      False -- asleep
      position -- position
      robotSpeed -- speed
      (RobotState Nothing initBrain position) -- Object Attributes

-------------
-- Sensing --
-------------

distanceToWall :: CardinalVector -> Point2D -> WallBound -> Maybe GLdouble
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

-----------
-- Think --
-----------

distanceToDouble :: Distance -> Double
distanceToDouble Infinite = 1000
distanceToDouble (Finite n) = n

senseToMatrix :: SensoryInput -> LA.L 1 6
senseToMatrix (SensoryInput n s e w (x, y)) =
  LA.matrix
    [ distanceToDouble n,
      distanceToDouble s,
      distanceToDouble e,
      distanceToDouble w,
      x,
      y
    ]

matrixToPoint :: LA.L 1 2 -> Point2D
matrixToPoint matrix =
  let r = LA.unrow matrix
      (x, rest) = LA.headTail r
      (y, _) = LA.headTail rest
   in (x, y)

think :: SensoryInput -> Brain -> Point2D
think input (Brain brain) = matrixToPoint (senseToMatrix input LA.<> brain)

---------
-- Act --
---------

updatePrevPosition :: Object -> Simulation ()
updatePrevPosition obj = do
  position <- getObjectPosition obj
  attribute <- getObjectAttribute obj
  case attribute of
    WallState _ -> pure ()
    RobotState time brain _ ->
      setObjectAttribute (RobotState time brain position) obj

act :: Point2D -> Object -> Simulation ()
act newSpeed obj = do
  setObjectSpeed newSpeed obj
  updatePrevPosition obj

----------
-- Step --
----------

stepRobot :: Object -> Simulation ()
stepRobot obj = do
  attribute <- getObjectAttribute obj
  case attribute of
    WallState _ -> pure ()
    RobotState _ brain _ -> do
      sensoryInput <- sense obj
      act (think sensoryInput brain) obj
