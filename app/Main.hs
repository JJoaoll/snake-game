module Main where

import Graphics.Gloss.Interface.IO.Game
    ( Display(FullScreen), playIO )
import Game.Flow ( updateGame )
import Game.Input ( handleInput )
import Game.Settings ( backgroundColor, initialGame, fps )
import Render ( drawGame )
import Game.Utils (genFreshStart)

-- total: 1800/1060
main :: IO ()
main = do
  freshStart <- genFreshStart
  playIO FullScreen backgroundColor fps freshStart drawGame handleInput updateGame
