module Main where

import Graphics.UI.Fungen hiding (Position)
import Graphics.Rendering.OpenGL (GLdouble, GLfloat)
import System.Random (randomRIO)
import Control.Monad (forM_)

type Name = String
type Dimentions = Point2D
type Position = Point2D
type Color = (GLfloat, GLfloat, GLfloat)

width, height :: Int
width = 800
height = 800

w, h :: GLdouble
w = fromIntegral width
h = fromIntegral height

invisMagenta :: [FilePath] -> [(FilePath, InvList)]
invisMagenta = map (\x -> (x, Just [(255, 0, 255)]))

main :: IO ()
main = do
    let winConfig = ((50, 50), (width, height), "Hello world")
    let gameMap = colorMap 0.0 0.0 0.0 250 250
    let room = objectGroup "roomGroup" createRoom
    robots <- objectGroup "robotGroup" <$> createRobots
    let bindings = [(Char 'q', Press, \_ _ -> funExit)]
    let bmpList = invisMagenta ["images/boom.bmp", "images/robot.bmp"]
    funInit
        winConfig -- main window layout
        gameMap -- background
        [room, robots] -- object groups
        () -- initial game state
        () -- initial game attribute
        bindings -- input bindings
        gameCycle -- step action
        (Timer 30) -- main loop timing
        bmpList -- image files

----------
-- INIT --
----------

createRobot :: Int -> IO (GameObject ())
createRobot index = do
    speed <- (,) <$> randomRIO (-5,5) <*> randomRIO (-5,5)
    pure $ object
        ("robot-" <> show index) -- name
        (Tex (25,25) 1) -- object picture
        False -- asleep
        (w/2, h/2) -- position
        speed -- speed
        () -- Object Attributes

createRobots :: IO [GameObject ()]
createRobots = mapM createRobot [1..10]

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
        roomWidth = 600

        roomHeight :: GLdouble
        roomHeight = 150

        thickness :: GLdouble
        thickness = 5

        doorWidth :: GLdouble
        doorWidth = 50

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

-------------
-- UPDATES --
-------------

explode :: GameObject s -> IOGame t s u v ()
explode obj = do
    replaceObject obj (updateObjectSize (100,100))
    setObjectCurrentPicture 0 obj
    setObjectSpeed (0,0) obj

---------------
-- GAME LOOP --
---------------

gameCycle :: IOGame () () () () ()
gameCycle = do
    showFPS TimesRoman24 (w - 40, 0) 1.0 0.0 0.0
    robots <- getObjectsFromGroup "robotGroup"
    forM_ robots $ \robot -> do
        -- Vertical wall collisions
        wall1 <- findObject "wall1" "roomGroup"
        wall3 <- findObject "wall3" "roomGroup"
        vColl <- objectListObjectCollision [wall1, wall3] robot
        when vColl (explode robot)
        -- Horizontal wall collisions
        wall2 <- findObject "wall2" "roomGroup"
        wall4 <- findObject "wall4" "roomGroup"
        wall5 <- findObject "wall5" "roomGroup"
        hColl <- objectListObjectCollision [wall2, wall4, wall5] robot
        when hColl (reverseYSpeed robot)

