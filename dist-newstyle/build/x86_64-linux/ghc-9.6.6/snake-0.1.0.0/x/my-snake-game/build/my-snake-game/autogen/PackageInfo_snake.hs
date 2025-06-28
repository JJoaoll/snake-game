{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_snake (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "snake"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A simple and classical snake game, but in Haskell. "
copyright :: String
copyright = ""
homepage :: String
homepage = ""
