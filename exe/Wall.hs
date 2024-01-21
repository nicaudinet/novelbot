{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Wall
  ( rectangleWall,
    intersectRectangleWall,
    intersectLine,
  )
where

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

intersectRectangleWall :: Line -> Point2D -> WallBound -> Bool
intersectRectangleWall line wallPos wallBound =
  let t = snd wallPos + top wallBound
      b = snd wallPos + bottom wallBound
      r = fst wallPos + right wallBound
      l = fst wallPos + left wallBound
      topIntersect = intersectLine line (Line (l, t) (r, t))
      bottomIntersect = intersectLine line (Line (l, b) (r, b))
      rightIntersect = intersectLine line (Line (r, b) (r, t))
      leftIntersect = intersectLine line (Line (l, b) (l, t))
   in topIntersect || bottomIntersect || rightIntersect || leftIntersect
