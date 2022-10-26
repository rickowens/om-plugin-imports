{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=OM.Plugin.Imports #-}
{-# OPTIONS_GHC -Wwarn #-}

module Main (main) where

import Data.List
import Data.Foldable
import Data.Maybe (Maybe(Just), fromJust)
import Data.Proxy (Proxy(..))
import GHC.Generics (from)
import GHC.Plugins (Plugin(pluginRecompile, typeCheckResultAction))
import GHC.Tc.Types (TcGblEnv(..))
import GHC.Hs
import qualified GHC.Generics as G


main :: IO ()
main =
  undefined
  Proxy
  fromJust
  (foldl' @[])
  (G.from @(Maybe Int))
  Just
  (from @(Maybe Int))
  typeCheckResultAction
  tcg_rn_imports
  ideclHiding
