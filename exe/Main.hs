module Main where

import Graphics.UI.Fungen hiding (Position)
import Graphics.Rendering.OpenGL (GLdouble, GLfloat)
import System.Random (randomRIO)
import Control.Monad (forM_)

type Name = String
type Dimentions = Point2D
type Position = Point2D
type Color = (GLfloat, GLfloat, GLfloat)

data ObjectState
    = RobotState
        { timeSinceBoom :: Maybe Int -- Timesteps since explosion
        }
    | WallState
type Object = GameObject ObjectState

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
    let walls = objectGroup "roomGroup" createWalls
    robots <- objectGroup "robotGroup" <$> createRobots
    let bindings = [(Char 'q', Press, \_ _ -> funExit)]
    let bmpList = invisMagenta ["images/boom.bmp", "images/robot.bmp"]
    funInit
        winConfig -- main window layout
        gameMap -- background
        [walls, robots] -- object groups
        () -- initial game state
        () -- initial game attribute
        bindings -- input bindings
        gameCycle -- step action
        (Timer 30) -- main loop timing
        bmpList -- image files

----------
-- INIT --
---------

createRobot :: Int -> IO Object
createRobot index = do
    speed <- (,) <$> randomRIO (-5,5) <*> randomRIO (-5,5)
    pure $ object
        ("robot-" <> show index) -- name
        (Tex (25,25) 1) -- object picture
        False -- asleep
        (w/2, h/2) -- position
        speed -- speed
        (RobotState Nothing) -- Object Attributes

createRobots :: IO [Object]
createRobots = mapM createRobot [1..10]

rectangleBound :: GLdouble -> GLdouble -> [Point2D]
rectangleBound roomWidth roomHeight =
    [ (-roomWidth / 2, -roomHeight / 2)
    , ( roomWidth / 2, -roomHeight / 2)
    , ( roomWidth / 2,  roomHeight / 2)
    , (-roomWidth / 2,  roomHeight / 2)
    ] -- CCW order!

createWall :: Int -> Dimentions -> Position -> Color -> Object
createWall index dims pos color =
    let
        bound = uncurry rectangleBound dims
        (r, g, b) = color
        picture = Basic (Polyg bound r g b Filled)
    in
        object ("wall-" <> show index) picture False pos (0,0) WallState 

createWalls :: [Object]
createWalls = [ wall1, wall2, wall3, wall4, wall5 ]
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

        wall1 :: Object
        wall1 =
            let dimentions = (thickness, roomHeight + thickness)
                position = (w / 2 - roomWidth / 2, h / 2)
            in createWall 1 dimentions position color

        wall2 :: Object
        wall2 =
            let dimentions = (roomWidth, thickness)
                position = (w / 2, h / 2 - roomHeight / 2)
            in createWall 2 dimentions position color

        wall3 :: Object
        wall3 =
            let dimentions = (thickness, roomHeight + thickness)
                position = (w / 2 + roomWidth / 2, h / 2)
            in createWall 3 dimentions position color

        wall4 :: Object
        wall4 =
            let dimentions = (30, thickness)
                position = (w / 2 + roomWidth / 2 - 15, h / 2 + roomHeight / 2)
            in createWall 4 dimentions position color

        wall5 :: Object
        wall5 =
            let dimentions = (roomWidth - 30 - doorWidth, thickness)
                position = (w / 2 - roomWidth / 2 + (fst dimentions) / 2, h / 2 + roomHeight / 2)
            in createWall 5 dimentions position color

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

gameCycle :: IOGame () ObjectState () () ()
gameCycle = do
    showFPS TimesRoman24 (w - 40, 0) 1.0 0.0 0.0
    robots <- getObjectsFromGroup "robotGroup"
    forM_ robots $ \robot -> do
        -- Vertical wall collisions
        wall1 <- findObject "wall-1" "roomGroup"
        wall3 <- findObject "wall-3" "roomGroup"
        vColl <- objectListObjectCollision [wall1, wall3] robot
        when vColl (explode robot)
        -- Horizontal wall collisions
        wall2 <- findObject "wall-2" "roomGroup"
        wall4 <- findObject "wall-4" "roomGroup"
        wall5 <- findObject "wall-5" "roomGroup"
        hColl <- objectListObjectCollision [wall2, wall4, wall5] robot
        when hColl (reverseYSpeed robot)

