{-# LANGUAGE UnicodeSyntax #-}
module Exec
  ( exec
  , exc
  ) where

import System.Directory (setCurrentDirectory)
import System.Process (waitForProcess, system)

import Control.Eternal.Syntax

exec :: String → IO()
exec args = system args ≫ return ()

exc :: String → String → IO()
exc path args = setCurrentDirectory path ≫ exec args
