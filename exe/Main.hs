module Main where

import Graphics.UI.Fungen hiding (Position)
import Graphics.Rendering.OpenGL (GLdouble, GLfloat)

type Name = String
type Dimentions = Point2D
type Position = Point2D
type Color = (GLfloat, GLfloat, GLfloat)

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
        room = objectGroup "roomGroup" createRoom
    in
        funInit
            winConfig -- main window layout
            gameMap -- background
            [ball, room] -- object groups
            () -- initial game state
            () -- initial game attribute
            bindings -- input bindings
            gameCycle -- step action
            (Timer 30) -- main loop timing
            [] -- image files

createBall :: GameObject ()
createBall =
    let
        ballPic = Basic (Circle 5.0 0.0 1.0 0.0 Filled)
    in
        object
            "ball" -- name
            ballPic -- object picture
            False -- asleep
            (w/2, h/2) -- position
            (-4,4) -- speed
            () -- Object Attributes

rectangleBound :: GLdouble -> GLdouble -> [Point2D]
rectangleBound roomWidth roomHeight =
    [ (-roomWidth / 2, -roomHeight / 2)
    , ( roomWidth / 2, -roomHeight / 2)
    , ( roomWidth / 2,  roomHeight / 2)
    , (-roomWidth / 2,  roomHeight / 2)
    ] -- CCW order!

createRectangle :: Name -> Dimentions -> Position -> Color -> GameObject ()
createRectangle name dims pos color =
    let
        bound = uncurry rectangleBound dims
        (r, g, b) = color
        picture = Basic (Polyg bound r g b Filled)
    in
        object name picture False pos (0,0) () 

createRoom :: [GameObject ()]
createRoom = [ wall1, wall2, wall3, wall4, wall5 ]
    where
        roomWidth :: GLdouble
        roomWidth = 200

        roomHeight :: GLdouble
        roomHeight = 50

        thickness :: GLdouble
        thickness = 2

        doorWidth :: GLdouble
        doorWidth = 30

        color :: Color
        color = (1.0, 1.0, 1.0) -- white

        wall1 :: GameObject ()
        wall1 =
            let dimentions = (thickness, roomHeight + thickness)
                position = (w / 2 - roomWidth / 2, h / 2)
            in createRectangle "wall1" dimentions position color

        wall2 :: GameObject ()
        wall2 =
            let dimentions = (roomWidth, thickness)
                position = (w / 2, h / 2 - roomHeight / 2)
            in createRectangle "wall2" dimentions position color

        wall3 :: GameObject ()
        wall3 =
            let dimentions = (thickness, roomHeight + thickness)
                position = (w / 2 + roomWidth / 2, h / 2)
            in createRectangle "wall3" dimentions position color

        wall4 :: GameObject ()
        wall4 =
            let dimentions = (30, thickness)
                position = (w / 2 + roomWidth / 2 - 15, h / 2 + roomHeight / 2)
            in createRectangle "wall4" dimentions position color

        wall5 :: GameObject ()
        wall5 =
            let dimentions = (roomWidth - 30 - doorWidth, thickness)
                position = (w / 2 - roomWidth / 2 + (fst dimentions) / 2, h / 2 + roomHeight / 2)
            in createRectangle "wall5" dimentions position color

gameCycle :: IOGame () () () () ()
gameCycle = do

    -- Get the ball object
    ball <- findObject "ball" "ballGroup"

    -- Vertical wall collisions
    wall1 <- findObject "wall1" "roomGroup"
    wall3 <- findObject "wall3" "roomGroup"
    vColl <- objectListObjectCollision [wall1, wall3] ball
    when vColl (reverseXSpeed ball)

    -- Horizontal wall collisions
    wall2 <- findObject "wall2" "roomGroup"
    wall4 <- findObject "wall4" "roomGroup"
    wall5 <- findObject "wall5" "roomGroup"
    hColl <- objectListObjectCollision [wall2, wall4, wall5] ball
    when hColl (reverseYSpeed ball)

    showFPS TimesRoman24 (w - 40, 0) 1.0 0.0 0.0
