{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# NoTypeAlias #-}


module Main where

import Control.Parallel.Strategies

import Graphics.Gloss.Interface.IO.Game
    -- ( Display(FullScreen), playIO )
import Game.Flow ( updateGame )
import Game.Input ( handleInput )
import Game.Settings
import Render ( drawGame )
import Game.Utils (genFreshStart)
import Game.Types

import Data.Kind 
import GHC.Float (int2Double)
import qualified Prelude as P
import Control.Lens
import Prelude hiding (Either(..), tail)
-- import Prelude (IO(..), Show(..), Eq(..), Bool(..), (.), ($), undefined, )
import Dir

-- total: 1800/1060
main :: IO ()
-- main = P.print "Hello, Haskell!"
main = do
  freshStart  <- genFreshStart AI
  runEval . rpar $ playIO (InWindow "snake" (100, 150) (0, 0)) backgroundColor fps freshStart drawGame handleInput updateGame
  freshStart' <- genFreshStart AI
  runEval . rpar $ playIO (InWindow "snake" (100, 150) (10, 10)) backgroundColor fps freshStart' drawGame handleInput updateGame

