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
  , ifadmin
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

setEnv :: String -- environment variable and value (x=5)
        → IO()
setEnv vvv = sys $ if | os ∈ ["win32", "mingw32"] → "set " ⧺ vvv
                      | os ∈ ["darwin", "cygwin32"] → "export " ⧺ vvv
                      | otherwise → "export " ⧺ vvv

ifadmin :: Bool   -- need sudo?
         → String -- root checker prefix
ifadmin adm =
  if | os ∈ ["win32", "mingw32"] → []
     | otherwise → if adm then "sudo "
                          else []

getMyMsGit :: MyEnv -- environment
            → Bool  -- if needs root
            → (String, String)
getMyMsGit myEnv adm = ( ifadm ⧺ myGit
                       , ifadm ⧺ "\"" ⧺ myGit ⧺ "\"" )
  where myGit :: String
        myGit = git myEnv

        ifadm :: String
        ifadm = ifadmin adm

-- Simple double Bool checker
chk :: IO (Bool, Bool) → (Bool, Bool)
     → IO (Bool, Bool)
chk foo (previous,doU) = if previous
        then return (previous, doU)
        else foo

-- Validate an folder exists and run action
vd :: String -- path to validate (for example .git)
    → String -- actual path
    → IO (Bool, Bool)
    → IO (Bool, Bool)
vd validator path action =
  doesDirectoryExist ⊲ path </> validator ≫= \vde →
          if vde then setCurrentDirectory path ≫ action
                 else return (True, False)
