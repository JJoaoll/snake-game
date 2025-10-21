{-# LANGUAGE TemplateHaskell #-}

module Game.Types where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game (Key)

data PlayerMode = Human | AI
  deriving (Show,Eq)

type Pos2D = (Int, Int)

data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Show,Eq)

data GameState = Playing | Pause | GameOver
  deriving (Show,Eq)

data Snake = Snake
  { _snakeBody :: [Pos2D]
  , _snakeSize :: Int
  , _lastDir   :: Direction 
  , _nextDir   :: Direction 
  } deriving (Eq, Show)

data Game = Game
  { _gameCharacter :: Snake
  , _gameFruitPos  :: Pos2D
  , _gameState     :: GameState
  , _lastKey       :: Key 
  , _playerMode    :: PlayerMode
  } deriving (Eq,Show)

$(makeLenses ''Snake)
$(makeLenses ''Game)
