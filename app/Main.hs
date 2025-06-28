module Main where

import Graphics.Gloss.Interface.IO.Game
    ( Display(FullScreen), playIO )
import Game.Flow ( updateGame )
import Game.Input ( handleInput )
import Game.Settings ( backgroundColor, initialGame, fps )
import Render ( drawGame )



-- total: 1800/1060
main :: IO ()
main = playIO FullScreen backgroundColor fps initialGame drawGame handleInput updateGame
-- main = play FullScreen black fps initialGame drawGame handleInput updateGame
