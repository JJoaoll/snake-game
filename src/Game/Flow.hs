module Game.Flow where


import Game.Types
    ( Direction(RIGHT, UP, DOWN, LEFT),
      Game(..),
      GameState(GameOver, Playing),
      Snake(..) )
import Game.Utils ( genPosThat )
import Game.Settings
    ( arena, upKeys, downKeys, leftKeys, rightKeys ) 


import Graphics.Gloss.Interface.IO.Game ( Key )


-- TODO: input bugs
updateSnakeDir :: Snake -> Graphics.Gloss.Interface.IO.Game.Key -> Snake
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
updateGame _ game@(Game snake@(Snake body size dir) fruit Playing)
  | h == fruit = do
      (a, b) <- genPosThat (`notElem` new_pos : body ++ arena)
      let snake' = snake { snake_body = body',
                          snake_size = size' }
      return game { game_character = snake', game_fruit = (a, b) }

  | h `elem` arena ++ t = return $ game { game_state = GameOver }
  | otherwise = return $
    let snake' = snake { snake_body = body',
                         snake_size = size' }
    in game {game_character = snake'}
       where h@(x, y) = head body
             t = drop 1 body

             size' = if h == fruit then size+1 else size
             body' = take size' (new_pos : body)

             new_pos = case dir of
                             UP    -> (x, y+1)
                             DOWN  -> (x, y-1)
                             LEFT  -> (x-1, y)
                             RIGHT -> (x+1, y)
updateGame _ game = return game
