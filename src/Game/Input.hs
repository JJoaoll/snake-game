module Game.Input where

import Game.Types ( Game(Game, game_character) )
import Game.Flow ( updateSnakeDir )

import Graphics.Gloss.Interface.IO.Game ( Event(EventKey) )

handleInput :: Event -> Game -> IO Game
handleInput (EventKey k _ _ _) g@(Game s _ _) =
  return $ g { game_character = updateSnakeDir s k}
handleInput _ w = return w
