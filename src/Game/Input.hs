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
    case _gameState g of 
      Pause   -> g { _gameState = Playing }
      Playing -> g { _gameState = Pause }
      _ -> g

handleInput (EventKey k Down _ _) g@(Game snake _ Playing _ _) =
  return $ g { _lastKey = k, _gameCharacter = updateSnakeDir snake k }
handleInput (EventKey k Down _ _) g@(Game _ _ GameOver _ _) =
  return $ g { _lastKey = k }
handleInput _ g = return g

