{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# NoTypeAlias #-}


module Main where

import Graphics.Gloss.Interface.IO.Game
    ( Display(FullScreen), playIO )
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
main = P.print "Hello, Haskell!"
-- main = do
--   freshStart <- genFreshStart
--   playIO FullScreen backgroundColor fps freshStart drawGame handleInput updateGame

-- data Dir 
--     = North 
--     | West
--     | East
--     | South 
--
--     | Northwest | Northeast
--     | Southwest | Southeast
--     deriving (Show, Eq)

