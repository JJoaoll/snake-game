module Game.Flow where

import Game.Types
import Game.Utils 
import Game.Settings
import Graphics.Gloss.Interface.IO.Game 

-- TODO: input bugs
updateSnakeDir :: Snake -> Key -> Snake
updateSnakeDir snake k
  | k `elem` upKeys    = safePut UP
  | k `elem` downKeys  = safePut DOWN
  | k `elem` leftKeys  = safePut LEFT
  | k `elem` rightKeys = safePut RIGHT
  | otherwise = snake
  where safePut dir
          | snake_dir snake /= op dir = snake { snake_dir = dir }
          | otherwise = snake
        op dir = case dir of
                   UP    -> DOWN
                   DOWN  -> UP
                   LEFT  -> RIGHT
                   RIGHT -> LEFT

updateGame :: Float -> Game -> IO Game
updateGame _ game@(Game snake@(Snake body size dir) fruit Playing _) = do
  print $ "Head: " ++ show h ++ ", Arena: " ++ show arena ++ ", Tail: " ++ show t
  if h == fruit then do 
      (a, b) <- genPosThat (`notElem` new_pos : body ++ arena)
      return game' { game_fruit = (a, b) }

  else if h `elem` arena ++ t then do 
    print "GameOver!!!"
    return game' { game_state = GameOver }

  else return game'

       where h@(x, y) = head body
             t = drop 1 body

             size' = if h == fruit then size+1 else size
             body' = take size' (new_pos : body)
             snake'= snake { snake_body = body',
                             snake_size = size' }
             game' = game {game_character = snake'}

             new_pos = case dir of
                             UP    -> (x, y+1)
                             DOWN  -> (x, y-1)
                             LEFT  -> (x-1, y)
                             RIGHT -> (x+1, y)

updateGame _ game@(Game _ _ GameOver k) =
  case k of 
    SpecialKey KeySpace -> genFreshStart
    _ -> return game

updateGame _ game = return game 

