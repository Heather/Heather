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

#if !( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
import System.Posix.User
#endif

setEnv :: String -- environment variable and value (x=5)
        → IO()
setEnv vvv = sys $ if | os ∈ ["win32", "mingw32"] → "set " ⧺ vvv
                      | os ∈ ["darwin", "cygwin32"] → "export " ⧺ vvv
                      | otherwise → "export " ⧺ vvv

ifadmin :: Bool   -- need sudo?
         → String -- root checker prefix
ifadmin adm =
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  [] -- TODO: check for administrator on windows
#else
  if adm then do isRoot <- fmap (== 0) getRealUserID
                 if isRoot then []
                           else "sudo "
         else []
#endif

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
-- once VCS is validated it's stored into YAML
vd :: String        -- path to validate (for example .git)
    → Maybe String  -- VCS (stored)
    → String        -- actual path
    → IO (Bool, Bool)
    → IO (Bool, Bool)
vd validator Nothing path action =
  doesDirectoryExist ⊲ path </> validator ≫= \vde →
          if vde then vcsupdate validator path
                       ≫ setCurrentDirectory path
                         ≫ action
                 else return (True, False)
vd validator vcx path action =
  if jvcx == validator then setCurrentDirectory path ≫ action
                       else return (True, False)
 where Just jvcx = vcx -- Nothing case guarded above
