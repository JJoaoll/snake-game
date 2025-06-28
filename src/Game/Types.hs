module Game.Types where

type Pos2D = (Int, Int)

data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Show,Eq)

data GameState = Playing | Pause | GameOver
  deriving (Show,Eq)

data Snake = Snake
  { snake_body :: [Pos2D]
  , snake_size :: Int
  , snake_dir  :: Direction }

data Game = Game
  { game_character :: Snake
  , game_fruit     :: Pos2D
  , game_state     :: GameState }
