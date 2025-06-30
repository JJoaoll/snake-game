module Render where

import Game
import Graphics.Gloss
import Data.Bifunctor ( Bifunctor(bimap) )

drawSnake :: Snake -> [Picture]
drawSnake (Snake [] _ _ _) = []
drawSnake snake@(Snake (h:t) _ _ _) =
  let (x, y) = bimap i2f i2f h in
    translate (x*69 - 935) (y*69 - 550) (color yellow $ squareSolid 50)
    : drawSnake snake { snake_body = t }

drawFruit :: Pos2D -> Picture
drawFruit (x, y) =
  translate (x'*69 - 935) (y'*69 - 550)
      (color blue $ circleSolid 25)
  where x'= i2f x
        y'= i2f y

drawGameScenario :: Game -> Picture
drawGameScenario (Game snake@(Snake _ score _ _) fruit _ _) =
     pictures $
       drawScore score :
       drawFruit fruit :
       drawArena : drawSnake snake

drawGame :: Game -> IO Picture
drawGame game@(Game _ _ Playing _) = return $
  drawGameScenario game

drawGame game@(Game _ _ GameOver _) = return $
  gameOverScreen <> drawGameScenario game

drawGame game@(Game snake fruit Pause _) = return $
  translate (-200) 0 (color white $ text "Pause")
    <> drawGameScenario game

drawScore :: Int -> Picture
drawScore score =
  translate (-820) 400 $
  scale 0.4 0.4 $
  color blue $ text $ show (score - 3)

gameOverScreen :: Picture
gameOverScreen  =
  playAgainTxt <> gameOverTxt

-- TODO: separate them to flexibility
gameOverTxt :: Picture
gameOverTxt =
  translate (-350) 0 $
    color white $ text "Game Over"

playAgainTxt :: Picture
playAgainTxt =
  scale 0.3 0.25 $
    translate (-800) (-300) $
      color white $ text "press SPC to play again"

-- empiric for better efficiency, sorry :c
drawArena :: Picture
drawArena = color red $
  translate (-4) (-467)  (rectangleSolid 1726 25)
  <> translate (-4) 473  (rectangleSolid 1726 25)
  <> translate (-854) 0 (rectangleSolid 25 ((866+859)/2 + 60))
  <> translate 847 0    (rectangleSolid 25 ((859+866)/2 + 60))
  -- <> pictures [color red (squareSolid 50) `drawnIn` (x, y) | (x, y) <- arena]
  --   pi
  --   pictures $ aux <$> arena
  -- -- pictures [color red (rectangleSolid 25 75) `drawIn` (x, y) | (x, y) <- arena, x == 1 || x == width]
  -- -- <> pictures [color red (rectangleSolid 75 25) `drawIn` (x, y) | (x, y) <- arena, y == 1 || y == width]
  -- where sq_line = color red $ rectangleSolid 1000 25
  --       sq_row  = color red $ rectangleSolid 25 1000
  --       aux (x, y)
  --         | (x, y) `elem` [(1, 1), (width, height)] =
  --           sq_row `drawnIn` (x, y) <> sq_line `drawnIn` (x, y)
  --         | x == 1 || x == width = sq_row `drawnIn` (x, y)
  --         | otherwise            = sq_line `drawnIn` (x, y)



drawnIn :: Picture -> Pos2D -> Picture
drawnIn picture (x, y) =
    translate (x'*69 - 935) (y'*69 - 550) picture
    where x' = i2f x
          y' = i2f y
