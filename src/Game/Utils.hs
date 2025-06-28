module Game.Utils where

import GHC.Float ( int2Float )

import Game.Types ( Pos2D )
import Game.Settings ( width, height )

import Graphics.Gloss ( rectangleSolid, Picture )
import System.Random (randomRIO)

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
