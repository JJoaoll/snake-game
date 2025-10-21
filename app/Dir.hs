{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}

module Dir where

import Graphics.Gloss.Interface.IO.Game
    ( Display(FullScreen), playIO )
import Game.Flow ( updateGame )
import Game.Input ( handleInput )
import Game.Settings
import Render ( drawGame )
import Game.Utils (genFreshStart)
import Game.Types

import Data.Kind 
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

-- data Dir 
--     = North 
--     | West
--     | East
--     | South 
--
--     | Northwest | Northeast
--     | Southwest | Southeast
--     deriving (Show, Eq)
data Dir 
    = Front 
    | Left
    | Right
    | Back

    | FrontLeft | FrontRight 
    | BackLeft  | BackRight 
    deriving (Show, Eq)

data Move 
    = TurnLeft
    | TurnRight
    | StayFront
    deriving (Show, Eq) 

clockWiseRot90 :: Dir -> Dir
clockWiseRot90 dir = 
  case dir of
    Front -> Right
    Right -> Back
    Back  -> Left
    Left  -> Front

    FrontLeft  -> FrontRight
    FrontRight -> BackRight
    BackRight  -> BackLeft
    BackLeft   -> FrontLeft

adjustedBy :: Dir -> Direction -> Dir
adjustedBy dir direction = 
  case direction of
    UP    -> dir
    RIGHT -> 
      clockWiseRot90 dir
    DOWN  -> 
      clockWiseRot90 $ 
      clockWiseRot90 dir
    LEFT  ->
      clockWiseRot90 $ 
      clockWiseRot90 $ 
      clockWiseRot90 dir

-- dir `redirect` StayFront = dir
--
-- UP    `redirect` TurnLeft = LEFT
-- LEFT  `redirect` TurnLeft = DOWN
-- DOWN  `redirect` TurnLeft = RIGHT
-- RIGHT `redirect` TurnLeft = UP
--
-- UP    `redirect` TurnRight = RIGHT
-- RIGHT `redirect` TurnRight = DOWN
-- DOWN  `redirect` TurnRight = LEFT
-- LEFT  `redirect` TurnRight = UP

data Sensor where
  WallAhead ::  Double -> Sensor 
  FoodAhead ::  Double -> Sensor 
  TailAhead ::  Double -> Sensor 
  deriving (Show, Eq)

data Operator 
    = LT | LEQ | GEQ | GT 
    -- | NotExists | Exists
  deriving (Show, Eq)

data DecisionTree
    = Action Move
    | Condition (Sensor, Dir) Operator Double DecisionTree DecisionTree
    deriving (Show, Eq)
-- width  = 26
-- height = 15



wallSensor :: Dir -> Game -> Double
wallSensor dir game = findFirstThat (`elem` arena) realDir headPos
  where 
    headPos:_ = game^.gameCharacter.snakeBody
    realDir = dir `adjustedBy` last_dir
    last_dir = game^.gameCharacter.lastDir

foodSensor :: Dir -> Game -> Maybe Double
foodSensor dir game = findFirstThatWhile (==fruitPos) (`elem` arena) realDir headPos
  where 
    headPos:_ = game^.gameCharacter.snakeBody
    realDir = dir `adjustedBy` last_dir
    last_dir = game^.gameCharacter.lastDir
    fruitPos = game^.gameFruitPos

tailSensor :: Dir -> Game -> Maybe Double
tailSensor dir game = findFirstThatWhile (`elem` tail) (`elem` arena) realDir headPos
  where 
    headPos:tail = game^.gameCharacter.snakeBody
    realDir = dir `adjustedBy` last_dir
    last_dir = game^.gameCharacter.lastDir

findFirstThat :: (Pos2D -> Bool) -> Dir -> Pos2D -> Double
findFirstThat p dir pos
  | p pos' = eucDiff pos pos'
  | otherwise = findFirstThat p dir pos'
  where pos' = pos `stepIn` dir

findFirstThatWhile :: (Pos2D -> Bool) -> (Pos2D -> Bool) -> Dir -> Pos2D -> Maybe Double
findFirstThatWhile p b dir pos -- b stays for "break"
  | b pos = Nothing
  | p pos' = eucDiff pos pos' & Just
  | otherwise = findFirstThatWhile p b dir pos'
  where pos' = pos `stepIn` dir

stepIn :: Pos2D -> Dir -> Pos2D
(x, y) `stepIn` dir = 
  case dir of -- inside here, theyre up down and whatever..
    Front -> (x, y+1)
    Right -> (x+1, y)
    Back  -> (x, y-1)
    Left  -> (x-1, y)

    FrontLeft  -> (x-1, y+1)
    FrontRight -> (x+1, y+1)
    BackRight  -> (x+1, y-1)
    BackLeft   -> (x-1, y-1)


-- the name for my "GameState" was "Game"

-- decideMove :: DecisionTree -> Game -> Direction
-- decideMove (Action move) game = (game^.gameCharacter.lastDir) `turn` move
-- decideMove (Condition sensor thenBranch elseBranch) game =
--     if evalSensor sensor game
--         then decideMove thenBranch game
--         else decideMove elseBranch game
--
-- calcTargetPosFrom :: Pos2D -> Dir -> Pos2D
-- calcTargetPosFrom (x, y) dir =
--     case dir of
--         North -> (x, y + 1)
--         South -> (x, y - 1)
--         East  -> (x + 1, y)
--         West  -> (x - 1, y)
--
--         Northeast -> (x + 1, y + 1)
--         Northwest -> (x - 1, y + 1)
--         Southeast -> (x + 1, y - 1)
--         Southwest -> (x - 1, y - 1)


eucDiff :: Pos2D -> Pos2D -> Double
eucDiff (x0, y0) (x1, y1) = sqrt $ dx**2 + dy**2
  where
    dx = fromIntegral x1 - fromIntegral x0
    dy = fromIntegral y1 - fromIntegral y0
    
evalWallSensor :: Game -> Direction -> Double
evalWallSensor game dir = undefined
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
