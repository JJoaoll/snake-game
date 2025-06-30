module Game.Utils where

import GHC.Float ( int2Float )

import Game.Types
import Game.Settings

import Graphics.Gloss ( rectangleSolid, Picture )
import System.Random (randomRIO)
import Graphics.Gloss.Interface.IO.Game

squareSolid :: Float -> Picture
squareSolid x = rectangleSolid x x

i2f :: Int -> Float
i2f = int2Float

diffPos2D :: Pos2D -> Pos2D -> Int
diffPos2D (x, y) (a, b) = abs (x - a) + abs (y - b)

-- TODO: while? until?
genPosThat :: (Pos2D -> Bool) -> IO Pos2D
genPosThat p =
  do x <- randomRIO (1, width)
     y <- randomRIO (1, height)
     if p (x, y) then return (x, y) else genPosThat p

randomDir :: IO Direction
randomDir = do n <- randomRIO (1 :: Int, 4)
               return $ case n of
                          1 -> UP
                          2 -> DOWN
                          3 -> LEFT
                          4 -> RIGHT

genFreshStart :: IO Game
genFreshStart =
  do x <- randomRIO (3, width-2)
     y <- randomRIO (3, height-2)
     dir <- randomDir
     let body = case dir of
                  UP    -> [(x, y), (x, y-1), (x,y-2)]
                  DOWN  -> [(x, y), (x, y+1), (x,y+2)]
                  LEFT  -> [(x, y), (x+1, y), (x+2,y)]
                  RIGHT -> [(x, y), (x-1, y), (x-2,y)]
     if and [diffPos2D (a, b) (u, v) >= 2 | (a, b) <- arena, (u, v) <- body] then do
       fruit_pos <- genPosThat (`notElem` body ++ arena) -- TODO: Generalize this fruit thing
       let snake = Snake body 3 dir dir
       return (Game snake fruit_pos Playing (SpecialKey KeyUnknown))

     else genFreshStart
