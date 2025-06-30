module Game.Settings where

import Game.Types
    ( Direction(RIGHT),
      Game(Game),
      GameState(Playing),
      Pos2D,
      Snake(Snake) )

import Graphics.Gloss.Interface.IO.Game

backgroundColor :: Color
backgroundColor = black

-- TODO: bad with performance
arena :: [Pos2D]
arena = [(x,y) | (x,y) <- gameMap, x ==  width || y == height || x == 1 || y == 1]

gameMap :: [Pos2D]
gameMap = [(x, y) | x <- [1 .. width], y <- [1 .. height]]

-- initialGame :: Game
-- initialGame = Game (Snake [(6,6),(5,6),(4,6)] 3 RIGHT) (10, 10) Playing (SpecialKey KeySpace)

upKeys, downKeys, leftKeys, rightKeys :: [Key]
upKeys    = [SpecialKey KeyUp   , Char 'w']
downKeys  = [SpecialKey KeyDown , Char 's']
leftKeys  = [SpecialKey KeyLeft , Char 'a']
rightKeys = [SpecialKey KeyRight, Char 'd']

fps, width, height :: Int
width  = 26
height = 15
fps    = 12
