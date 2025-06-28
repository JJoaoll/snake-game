module Render where

import Game.Settings ( arena )
import Game.Types
    ( Game(Game),
      GameState(Pause, Playing, GameOver),
      Pos2D,
      Snake(Snake, snake_body) )
import Game.Utils ( squareSolid, i2f ) 
import Graphics.Gloss
    ( blue,
      red,
      white,
      yellow,
      circle,
      circleSolid,
      color,
      pictures,
      translate,
      Picture )
import Data.Bifunctor ( Bifunctor(bimap) )

drawSnake :: Snake -> [Picture]
drawSnake (Snake [] _ _) = []
drawSnake snake@(Snake (h:t) _ _) =
  let (x, y) = bimap i2f i2f h in
    translate (x*89 - 935) (y*69 - 550) (color yellow $ squareSolid 50)
    : drawSnake snake { snake_body = t }


drawFruit :: Pos2D -> Picture
drawFruit (x, y) =
  translate (x'*89 - 935) (y'*69 - 550)
      (color blue $ circleSolid 25)
  where x'= i2f x
        y'= i2f y

drawGame :: Game -> IO Picture
drawGame (Game snake fruit Playing) = return $
     pictures $
       drawFruit fruit : drawArena : drawSnake snake

drawGame (Game _ _ GameOver) = return $ color white $ circleSolid 50
drawGame (Game _ _ Pause)    = return $ circle 50
  -- pictures $ [ putIn (x, y) | (x, y) <- gameMap ]
  -- ++ drawSnake snake
  -- where putIn (x, y) =
  --         translate (x'*89 - 935) (y'*69 - 550) $
  --         color red $
  --         squareSolid 50
  --        where x'= int2Float x
  --              y'= int2Float y


drawArena :: Picture
drawArena = pictures [color red (squareSolid 50) `drawIn` (x, y) | (x, y) <- arena]

drawIn :: Picture -> Pos2D -> Picture
drawIn picture (x, y) =
    translate (x'*89 - 935) (y'*69 - 550) picture
    where x' = i2f x
          y' = i2f y
