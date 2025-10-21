{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}

module Dir where

import Graphics.Gloss.Interface.IO.Game
    ( Display(FullScreen), playIO )
-- import Game.Flow ( updateGame )
-- import Game.Input ( handleInput )
import Game.Settings
-- import Render ( drawGame )
-- import Game.Utils (genFreshStart)
import Game.Types

import Data.Kind 
import GHC.Float (int2Double)
import qualified Prelude as P
import Control.Lens
import Prelude hiding (Either(..), tail, LT, GT)
-- import Prelude (IO(..), Show(..), Eq(..), Bool(..), (.), ($), undefined, )

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

data Sensor 
    = WallAhead 
    | FoodAhead
    | TailAhead
  deriving (Show, Eq)

-- i know its not an operation tho..
data Operator 
    = LT | LEQ | GEQ | GT 
  deriving (Show, Eq)

data DecisionTree
    = Action Move
    | Condition (Sensor, Dir) Operator Double DecisionTree DecisionTree
    deriving (Show, Eq)

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
  case dir of 
    Front -> (x, y+1)
    Right -> (x+1, y)
    Back  -> (x, y-1)
    Left  -> (x-1, y)

    FrontLeft  -> (x-1, y+1)
    FrontRight -> (x+1, y+1)
    BackRight  -> (x+1, y-1)
    BackLeft   -> (x-1, y-1)

eucDiff :: Pos2D -> Pos2D -> Double
eucDiff (x0, y0) (x1, y1) = sqrt $ dx**2 + dy**2
  where
    dx = fromIntegral x1 - fromIntegral x0
    dy = fromIntegral y1 - fromIntegral y0
    
evalWallSensor :: Game -> Direction -> Double
evalWallSensor game dir = undefined
