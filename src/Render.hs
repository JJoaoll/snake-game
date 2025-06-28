module Render where

import Game
import Graphics.Gloss
import Data.Bifunctor ( Bifunctor(bimap) )

drawSnake :: Snake -> [Picture]
drawSnake (Snake [] _ _) = []
drawSnake snake@(Snake (h:t) _ _) =
  let (x, y) = bimap i2f i2f h in
    translate (x*69 - 935) (y*69 - 550) (color yellow $ squareSolid 50)
    : drawSnake snake { snake_body = t }

drawFruit :: Pos2D -> Picture
drawFruit (x, y) =
  translate (x'*69 - 935) (y'*69 - 550)
      (color blue $ circleSolid 25)
  where x'= i2f x
        y'= i2f y

drawGame :: Game -> IO Picture
drawGame (Game snake fruit Playing _) = return $
     pictures $
       drawFruit fruit : drawArena : drawSnake snake

drawGame (Game _ _ GameOver _) = return gameOverScreen
drawGame (Game snake fruit Pause _)    = return $
     pictures $ translate (-200) 0 (color white $ text "Pause") :
       drawFruit fruit : drawArena : drawSnake snake

  -- pictures $ [ putIn (x, y) | (x, y) <- gameMap ]
  -- ++ drawSnake snake
  -- where putIn (x, y) =
  --         translate (x'*89 - 935) (y'*69 - 550) $
  --         color red $
  --         squareSolid 50
  --        where x'= int2Float x
  --              y'= int2Float y

gameOverScreen :: Picture
gameOverScreen =
  playAgainTxt <> gameOverTxt

gameOverTxt :: Picture
gameOverTxt =
  translate (-350) 0 $
    color white $ text "Game Over"

playAgainTxt :: Picture
playAgainTxt =
  scale 0.3 0.25 $
    translate (-800) (-300) $
      color white $ text "press SPC to play again"

drawArena :: Picture
drawArena = pictures [color red (squareSolid 50) `drawIn` (x, y) | (x, y) <- arena]

drawIn :: Picture -> Pos2D -> Picture
drawIn picture (x, y) =
    translate (x'*69 - 935) (y'*69 - 550) picture
    where x' = i2f x
          y' = i2f y
