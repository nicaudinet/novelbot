{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Wall
  ( rectangleWall,
    intersectRectangleWall,
    intersectLine,
  )
where

import Data.Maybe (catMaybes)
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen
import Types (Color, Line (..), Object, ObjectState (WallState), WallBound (..))

boundToList :: WallBound -> [Point2D]
boundToList WallBound {..} =
  [ (right, top),
    (left, top),
    (left, bottom),
    (right, bottom)
  ] -- counter-clockwise order

rectangleBound :: GLdouble -> GLdouble -> WallBound
rectangleBound roomWidth roomHeight =
  WallBound
    { top = roomHeight / 2,
      bottom = -roomHeight / 2,
      right = roomWidth / 2,
      left = -roomWidth / 2
    }

rectangleWall :: Int -> Point2D -> Point2D -> Color -> Object
rectangleWall index dims pos color =
  let bound = uncurry rectangleBound dims
      (r, g, b) = color
      picture = Basic (Polyg (boundToList bound) r g b Filled)
   in object ("wall-" <> show index) picture False pos (0, 0) (WallState bound)

-- Got this solution from here:
-- https://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/
intersectLine :: Line -> Line -> Bool
intersectLine (Line a b) (Line c d) =
  ccw a c d /= ccw b c d && ccw a b c /= ccw a b d
  where
    ccw :: Point2D -> Point2D -> Point2D -> Bool
    ccw (ax, ay) (bx, by) (cx, cy) =
      (cy - ay) * (bx - ax) > (by - ay) * (cx - ax)

-- (x1,y1) -- (x2,y2)  => y = m1 * x + c1
-- m1 = (y2 - y1) / (x2 - x1)
-- c1 = y1 - m1 * x1
--
-- (a1,b1) -- (a2,b2)  => b = m2 * a + c2
-- m2 = (b2 - b1) / (a2 - a1)
-- c2 = b1 - m2 * a1
--
-- Intersection when:
-- y = b
-- => m1 * x + c1 = m2 * x + c2
-- => x * (m1 - m2) = c2 - c1
-- => x = (c2 - c1) / (m1 - m2)
-- => y = m1 * x + c1
intersectLinePoint :: Line -> Line -> Maybe Point2D
intersectLinePoint line1 line2 =
  if not (intersectLine line1 line2)
    then Nothing
    else
      let Line (x1, y1) (x2, y2) = line1
          Line (a1, b1) (a2, b2) = line2
          m1 = (y2 - y1) / (x2 - x1)
          c1 = y1 - m1 * x1
          m2 = (b2 - b1) / (a2 - a1)
          c2 = b1 - m2 * a1
          x = (c2 - c1) / (m1 - m2)
          y = m1 * x + c1
       in Just (x, y)

intersectRectangleWall :: Line -> Point2D -> WallBound -> [Point2D]
intersectRectangleWall line wallPos wallBound =
  let t = snd wallPos + top wallBound
      b = snd wallPos + bottom wallBound
      r = fst wallPos + right wallBound
      l = fst wallPos + left wallBound
      topSide = Line (l, t) (r, t)
      bottomSide = Line (l, b) (r, b)
      rightSide = Line (r, b) (r, t)
      leftSide = Line (l, b) (l, t)
      sides = [topSide, bottomSide, rightSide, leftSide]
      intersections = map (intersectLinePoint line) sides
   in catMaybes intersections
