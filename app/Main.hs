module Main where

import Graphics.Gloss.Interface.IO.Game
    ( Display(FullScreen), playIO )
import Game.Flow ( updateGame )
import Game.Input ( handleInput )
import Game.Settings
import Render ( drawGame )
import Game.Utils (genFreshStart)
import Game.Types

-- total: 1800/1060
main :: IO ()
main = print "Hello, Haskell!"
-- main = do
--   freshStart <- genFreshStart
--   playIO FullScreen backgroundColor fps freshStart drawGame handleInput updateGame

data Dir 
    = North 
    | West
    | East
    | South 
    | Northwest | Northeast
    | Southwest | Southeast
    deriving (Show, Eq)

data Move 
    = Left
    | Right
    | Front
    deriving (Show, Eq) 

data Sensor
    = IsWallAhead Dir
    | IsFoodAhead Dir
    | IsTailAhead Dir  
    deriving (Show, Eq)

data DecisionTree
    = Action Move
    | Condition Sensor DecisionTree DecisionTree  
    deriving (Show, Eq)

-- the name for my "GameState" was "Game"
evaluateSensor :: Sensor -> Game -> Bool
-- evaluateSensor IsWallAhead gs = 
--     let (x,y) = head (snakeBody gs)
--         (w,h) = worldSize gs
--     in case direction gs of
--         North -> y == 0
--         South -> y == h - 1
--         West  -> x == 0
--         East  -> x == w - 1
evaluateSensor _ _ = False -- Placeholder

decideMove :: DecisionTree -> Game -> Direction
decideMove (Action move) Game = move
decideMove (Condition sensor trueBranch falseBranch) gameState =
    if evaluateSensor sensor gameState
        then decideMove trueBranch gameState
        else decideMove falseBranch gameState

-- type Individual = (DecisionTree, Double)
-- type Population = [Individual]
