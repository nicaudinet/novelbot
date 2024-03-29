{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Brain
  ( steer,
  )
where

import Control.Monad (forM)
import Data.Maybe (catMaybes)
import GHC.TypeLits ()
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen
import qualified Numeric.LinearAlgebra.Static as LA (L, headTail, matrix, unrow, (<>))
import Types (Object, ObjectState (WallState), Simulation, WallBound (..))

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

type Brain = LA.L 6 2

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

initBrain :: Brain
initBrain = LA.matrix (replicate 12 0.001)

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
think input brain = matrixToPoint (senseToMatrix input LA.<> brain)

-- lessThan :: Distance -> Double -> Bool
-- lessThan Infinite _ = True
-- lessThan (Finite a) b = a < b
--
-- thinkDoNotHitWalls :: SensoryInput -> Brain -> Point2D
-- thinkDoNotHitWalls (SensoryInput n s e w (x, y)) _brain
--   | x <= 0 && w `lessThan` dist = (strength, 0)
--   | x > 0 && e `lessThan` dist = (-strength, 0)
--   | y <= 0 && s `lessThan` dist = (0, strength)
--   | y > 0 && n `lessThan` dist = (0, -strength)
--   | otherwise = (0, 0)
--   where
--     dist, strength :: Double
--     dist = 30
--     strength = 1

act :: Point2D -> Object -> Simulation ()
act = setObjectSpeed

-- SENSE -> THINK -> ACT loop
steer :: Object -> Simulation ()
steer obj = do
  sensoryInput <- sense obj
  act (think sensoryInput initBrain) obj
