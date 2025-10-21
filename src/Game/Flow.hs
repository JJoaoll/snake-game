module Game.Flow where

import Game.Types
import Game.Utils 
import Game.Settings
import Graphics.Gloss.Interface.IO.Game 

import Control.Lens ((^.))
import Dir
import qualified Dir as Dir
import Brain


-- TODO: input bugs
updateSnakeDir :: Snake -> Key -> Snake
updateSnakeDir snake k
  | k `elem` upKeys    = put UP
  | k `elem` downKeys  = put DOWN
  | k `elem` leftKeys  = put LEFT
  | k `elem` rightKeys = put RIGHT
  | otherwise = snake
  where put dir = snake { _nextDir = dir }


safeRedirect :: Direction -> Direction -> Direction
from `safeRedirect` to 
  | from == op to = from
  | otherwise     = to
        where op dir = case dir of
                   UP    -> DOWN
                   DOWN  -> UP
                   LEFT  -> RIGHT
                   RIGHT -> LEFT

type Seconds = Float
updateGame :: Seconds -> Game -> IO Game
updateGame _ game@(Game snake@(Snake body size from to) fruit Playing _ mode) =
    do if h' == fruit then do
         (a, b) <- genPosThat (`notElem` h' : body ++ arena)
         return game' { _gameFruitPos = (a, b) }

        else if h' `elem` arena ++ snk_tail then
          return game' { _gameState = GameOver }

        else return game'

  where (x, y)   = head body
        snk_tail = drop 1 body

        dir'  = case mode of 
                  Human -> from `safeRedirect` to
                  AI -> from `turn` decide game brainTest
        size' = if h' == fruit then size+1 else size
        body' = take size' (h' : body)
        snake'= snake { _snakeBody = body',
                        _snakeSize = size',
                        _lastDir = dir' }

        game' = game {_gameCharacter = snake'}
        h'    = case dir' of
                  UP    -> (x, y+1)
                  DOWN  -> (x, y-1)
                  LEFT  -> (x-1, y)
                  RIGHT -> (x+1, y)

updateGame _ game@(Game _ _ GameOver k _) =
  case k of 
    SpecialKey KeySpace -> genFreshStart (game^.playerMode)
    _ -> return game

updateGame _ game = return game 



---- new ai things
decide :: Game -> DecisionTree -> Move
decide _game (Action move) = move
decide game (Condition (sensor, dir) op bounder dtThen dtElse) = 
  case sensor' dir game of
    Nothing 
      | op `elem` [Dir.GEQ, Dir.GT] -> decide game dtThen
      | otherwise           -> decide game dtElse
    Just dlt 
      | dlt `op'` bounder -> decide game dtThen
      | otherwise         -> decide game dtElse
  where 
    sensor' = case sensor of 
                  WallAhead -> \d g -> 
                    Just $ 
                    wallSensor d g 
                  FoodAhead -> foodSensor
                  TailAhead -> tailSensor
    op' :: Double -> Double -> Bool
    op' = case op of
              Dir.LT  -> (<)
              Dir.LEQ -> (<=)
              Dir.GEQ -> (>=)
              Dir.GT  -> (>)
    

