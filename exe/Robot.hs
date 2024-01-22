{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Robot
  ( createRobot,
    stepRobot,
    explode,
  )
where

import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import GHC.TypeLits ()
import Graphics.UI.Fungen
import Numeric.LinearAlgebra.Static (randn)
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

-- initBrain :: Brain
-- initBrain = Brain $ LA.matrix (replicate 12 0.0000001)

randomBrain :: IO Brain
randomBrain = Brain <$> randn

createRobot :: Point2D -> Int -> IO Object
createRobot (ww, wh) index = do
  robotSpeed <- (,) <$> randomRIO (-5, 5) <*> randomRIO (-5, 5)
  let position = (ww / 2, wh / 2)
  brain <- randomBrain
  pure $
    object
      ("robot-" <> show index) -- name
      (Tex (25, 25) 1) -- object picture
      False -- asleep
      position -- position
      robotSpeed -- speed
      (RobotState 0 Nothing brain position) -- Object Attributes

-------------
-- Sensing --
-------------

distanceToWall :: CardinalVector -> Point2D -> WallBound -> Distance
distanceToWall (CardinalVector (x, y) direction) wallPos wallBound =
  let t = snd wallPos + top wallBound
      b = snd wallPos + bottom wallBound
      r = fst wallPos + right wallBound
      l = fst wallPos + left wallBound
   in case direction of
        North ->
          if y < b && l < x && x < r
            then Finite (b - y)
            else PosInfinite
        South ->
          if y > t && l < x && x < r
            then Finite (y - t)
            else NegInfinite
        East ->
          if x < l && b < y && y < t
            then Finite (l - x)
            else PosInfinite
        West ->
          if x > r && b < y && y < t
            then Finite (x - r)
            else NegInfinite

minAbsDistance :: [Distance] -> Maybe Double
minAbsDistance [] = Nothing
minAbsDistance (PosInfinite : xs) = minAbsDistance xs
minAbsDistance (NegInfinite : xs) = minAbsDistance xs
minAbsDistance (Finite x : xs) =
  case minAbsDistance xs of
    Nothing -> Nothing
    Just y -> Just (min x y)

distanceToWalls :: CardinalVector -> Simulation (Maybe Double)
distanceToWalls vec = do
  walls <- getObjectsFromGroup "roomGroup"
  distances <- forM walls $ \wall -> do
    wallPos <- getObjectPosition wall
    WallState wallBound <- getObjectAttribute wall
    pure (distanceToWall vec wallPos wallBound)
  pure (minAbsDistance distances)

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

senseToMatrix :: SensoryInput -> LA.L 1 6
senseToMatrix (SensoryInput n s e w (x, y)) =
  LA.matrix
    [ fromMaybe 10e3 n,
      fromMaybe 10e3 s,
      fromMaybe 10e3 e,
      fromMaybe 10e3 w,
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

act :: Point2D -> Object -> Simulation ()
act acc obj = do
  attribute <- getObjectAttribute obj
  case attribute of
    WallState _ -> pure ()
    robot ->
      case timeSinceBoom robot of
        Nothing -> do
          -- Accelerate
          robotSpeed <- getObjectSpeed obj
          let newSpeed = (fst robotSpeed + fst acc, snd robotSpeed + snd acc)
          setObjectSpeed newSpeed obj
          -- Update previous position
          pos <- getObjectPosition obj
          let newRobot =
                robot {prevPos = pos, timeSinceStart = 1 + timeSinceStart robot}
          setObjectAttribute newRobot obj
        Just n -> do
          setObjectAttribute (robot {timeSinceBoom = Just (n + 1)}) obj
          if n < 30
            then do
              (sx, sy) <- getObjectSize obj
              replaceObject obj (updateObjectSize (sx + 1, sy + 1))
            else do
              setObjectCurrentPicture 0 obj
              setObjectAsleep True obj

explode :: Point2D -> Object -> Simulation ()
explode pos obj = do
  attribute <- getObjectAttribute obj
  case attribute of
    WallState _ -> pure ()
    robot -> do
      replaceObject obj (updateObjectSize (100, 100))
      setObjectCurrentPicture 2 obj
      setObjectSpeed (0, 0) obj
      setObjectPosition pos obj
      let newRobot = robot {timeSinceBoom = Just 0, prevPos = pos}
      setObjectAttribute newRobot obj

----------
-- Step --
----------

stepRobot :: Object -> Simulation ()
stepRobot obj = do
  attribute <- getObjectAttribute obj
  case attribute of
    WallState _ -> pure ()
    robot -> do
      sensoryInput <- sense obj
      act (think sensoryInput (robotBrain robot)) obj
