module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data Robot = Robot Point2D

width, height :: Int
width = 400
height = 400

w, h :: GLdouble
w = fromIntegral width
h = fromIntegral height

main :: IO ()
main =
    let
        winConfig = ((50, 50), (width, height), "Hello world")
        gameMap = colorMap 0.0 0.0 0.0 250 250
        bindings = [(Char 'q', Press, \_ _ -> funExit)]
        ball = objectGroup "ballGroup" [createBall]
    in
        funInit
            winConfig -- main window layout
            gameMap -- background
            [ball] -- object groups
            () -- initial game state
            () -- initial game attribute
            bindings -- input bindings
            gameCycle -- step action
            (Timer 30) -- main loop timing
            [] -- image files

createBall :: GameObject ()
createBall =
    let ballPic = Basic (Circle 10.0 0.0 1.0 0.0 Filled)
    in object "ball" ballPic False (w/2, h/2) (-8,8) ()

gameCycle :: IOGame () () () () ()
gameCycle = do
    -- Get the ball object
    ball <- findObject "ball" "ballGroup"

    -- Deal with collisions
    collision_l <- objectLeftMapCollision ball
    collision_r <- objectRightMapCollision ball
    collision_t <- objectTopMapCollision ball
    collision_b <- objectBottomMapCollision ball
    when (collision_l || collision_r) (reverseXSpeed ball)
    when (collision_t || collision_b) (reverseYSpeed ball)

    showFPS TimesRoman24 (w - 40, 0) 1.0 0.0 0.0
