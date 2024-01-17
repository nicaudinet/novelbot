{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Wall
  ( rectangleWall,
  )
where

import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen
import Types (Color, Object, ObjectState (WallState), WallBound (..))

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
