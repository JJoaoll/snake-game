module Game.Input where

import Game.Types
import Game.Flow 
import System.Exit (exitSuccess)
import Graphics.Gloss.Interface.IO.Game 

handleInput :: Event -> Game -> IO Game
handleInput (EventKey (SpecialKey KeyEsc) _ _ _) _ =
  exitSuccess

-- TODO: fix this bad solution..
handleInput (EventKey (Char 'p') Down _ _) g = 
  return $ 
    case game_state g of 
      Pause   -> g { game_state = Playing }
      Playing -> g { game_state = Pause }
      _ -> g 

handleInput (EventKey k Down _ _) g@(Game s _ Playing _) =
  return $ g { game_character = updateSnakeDir s k, last_key = k }
handleInput (EventKey k Down _ _) g@(Game _ _ GameOver _) =
  return $ g { last_key = k }
handleInput _ g = return g
