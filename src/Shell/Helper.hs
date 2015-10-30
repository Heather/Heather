{-# LANGUAGE
    MultiWayIf
  , LambdaCase
  , CPP
  , UnicodeSyntax
  #-}

module Shell.Helper
  ( setEnv
  , getMyMsGit
  , chk
  , vd
  ) where

import Data.List.Split
import Data.Maybe
import Data.List

import System.Info (os)
import System.Directory
import System.FilePath((</>))

import System.Process

import Trim
import Exec
import Config

setEnv :: String → IO()
setEnv vvv = sys $ if | os ∈ ["win32", "mingw32"] → "set " ⧺ vvv
                      | os ∈ ["darwin", "cygwin32"] → "export " ⧺ vvv
                      | otherwise → "export " ⧺ vvv

getMyMsGit :: MyEnv → (String, String)
getMyMsGit myEnv = (myGit, "\"" ⧺ myGit ⧺ "\"")
  where myGit = git myEnv

-- Simple double Bool checker
chk :: IO (Bool, Bool) → (Bool, Bool)
     → IO (Bool, Bool)
chk foo (previous,doU) = if previous
        then return (previous, doU)
        else foo

-- Validate an folder exists and run action
vd :: String → String → IO (Bool, Bool) → IO (Bool, Bool)
vd validator path action =
  doesDirectoryExist ⊲ path </> validator ≫= \vde →
          if vde then setCurrentDirectory path ≫ action
                 else return (True, False)
