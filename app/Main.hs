module Main where

import Graphics.Gloss.Interface.IO.Game
    ( Display(FullScreen), playIO )
import Game.Flow ( updateGame )
import Game.Input ( handleInput )
import Game.Settings
import Render ( drawGame )
import Game.Utils (genFreshStart)
import Game.Types

import GHC.Float (int2Double)
import qualified Prelude as P
import Control.Lens
import Prelude hiding (Either(..), tail)
-- import Prelude (IO(..), Show(..), Eq(..), Bool(..), (.), ($), undefined, )

-- total: 1800/1060
main :: IO ()
main = P.print "Hello, Haskell!"
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

turn :: Direction -> Move -> Direction
dir `turn` Front = dir

UP    `turn` Left = LEFT
LEFT  `turn` Left = DOWN
DOWN  `turn` Left = RIGHT
RIGHT `turn` Left = UP

UP    `turn` Right = RIGHT
RIGHT `turn` Right = DOWN
DOWN  `turn` Right = LEFT
LEFT  `turn` Right = UP

data Sensor
    = IsWallAhead Dir
    | IsFoodAhead Dir
    | IsTailAhead Dir  
    deriving (Show, Eq)

data Operator 
    = LT | LEQ | GEQ | GT 
    | NotExists | Exists
  deriving (Show, Eq)

data DecisionTree
    = Action Move
    | Condition Sensor Operator Double DecisionTree DecisionTree
    deriving (Show, Eq)

-- the name for my "GameState" was "Game"

-- decideMove :: DecisionTree -> Game -> Direction
-- decideMove (Action move) game = (game^.gameCharacter.lastDir) `turn` move
-- decideMove (Condition sensor thenBranch elseBranch) game =
--     if evalSensor sensor game
--         then decideMove thenBranch game
--         else decideMove elseBranch game
--
calcTargetPosFrom :: Pos2D -> Dir -> Pos2D
calcTargetPosFrom (x, y) dir =
    case dir of
        North -> (x, y + 1)
        South -> (x, y - 1)
        East  -> (x + 1, y)
        West  -> (x - 1, y)

        Northeast -> (x + 1, y + 1)
        Northwest -> (x - 1, y + 1)
        Southeast -> (x + 1, y - 1)
        Southwest -> (x - 1, y - 1)


eucDiff :: Pos2D -> Pos2D -> Double
eucDiff (x0, y0) (x1, y1) = sqrt $ dx**2 + dy**2
  where
    dx = fromIntegral x1 - fromIntegral x0
    dy = fromIntegral y1 - fromIntegral y0
    
-- posDiff :: Pos2D -> Pos2D -> (Maybe Dir, Double)
-- posDiff (x0, y0) (x1, y1) 
--   | x0 == x1 && y0 == y1 = (Nothing, 0)
--   | x0 /= x1 && y0 /= y1 
--   = let 
--       ver | x0 < x1 = UP    | otherwise = DOWN 
--       hor | y0 < y1 = RIGHT | otherwise = LEFT
--       x0' = int2Double x0
--       x1' = int2Double x1
--       y0' = int2Double y0
--       y1' = int2Double y1
--
--       verDelta = sqrt $ max(x0', x1') ** 2 - min(x0', x1') ** 2
--       horDelta = sqrt $ max(y0', y1') ** 2 - min(y0', y1') ** 2
--     in case (ver, hor) of
--       (UP, LEFT)   -> (Just Northwest, sqrt ( ^^ 2))
--       (UP, RIGHT)  -> (Just Northeast, undefined)
--       (DOWN, LEFT) -> (Just Southwest, undefined)
--       (DOWN, RIGHT)-> (Just Southeast, undefined)
--       _ -> error ""
--   | x0 /= x1  = undefined
--   | y0 /= y1  = undefined
--   | otherwise = undefined
    

-- evalWallSensor :: Direction -> Game -> Int
-- evalWallSensor dir game = raycast 1 (calcTargetPosFrom dir) dir arena
--   where
--     (snake_x, snake_y):_ = game^.gameCharacter.snakeBody
--     calculateTargetPos = calcTargetPosFrom (snake_x, snake_y)
--
-- evalTailSensor :: Direction -> Game -> Maybe Int
-- evalTailSensor dir game = raycast 1 (calculateTargetPos dir) dir tail
--   where
--     (snake_x, snake_y):tail = game^.gameCharacter.snakeBody
--     calculateTargetPos = calculateTargetPosFrom (snake_x, snake_y)
--
-- -- Função 3: Especialista em detectar a fruta
-- evalFruitSensor :: Direction -> Game -> Maybe Int
-- evalFruitSensor dir game =
--   let
--     (snake_x, snake_y):_ = game^.gameCharacter.snakeBody
--     (fruit_x, fruit_y) = game^.gameFruitPos
--     delta_x = fruit_x - snake_x
--     delta_y = fruit_y - snake_y
--   in
--     if isAligned dir (delta_x, delta_y)
--     then Just (abs delta_x + abs delta_y)
--     else Nothing
--   where
--     -- A função 'isAligned' seria definida aqui dentro.
--     isAligned :: Direction -> (Int, Int) -> Bool
--     isAligned d (dx, dy) = undefined  
--
-- -- Ela atua como um "roteador", direcionando para a função correta.
-- -- evalSensor :: Sensor -> Game -> Maybe Int
-- -- evalSensor sensor game =
-- --   case sensor of
-- --     IsWallAhead dir -> evalWallSensor dir game
-- --     IsTailAhead dir -> evalTailSensor dir game
-- --     IsFruitAhead dir -> evalFruitSensor dir game
