{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_chat_app (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "chat_app"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "chat application"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
