{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE UnicodeSyntax  #-}

module Shell.Helper
  ( setEnv
  , getMyMsGit
  , chk
  , vd
  , ifadmin
  ) where

import           Data.List.Split
import           Data.Maybe
import           Data.List

import           System.Info (os)
import           System.Directory
import           System.FilePath((</>))

import           System.Process

import           Trim
import           Exec
import           Config

#if !( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
import           System.Posix.User
#endif

setEnv :: String -- environment variable and value (x=5)
        → IO()
setEnv vvv = sys $ if | os ∈ ["win32", "mingw32"] → "set " ⧺ vvv
                      | os ∈ ["darwin", "cygwin32"] → "export " ⧺ vvv
                      | otherwise → "export " ⧺ vvv

ifadmin :: Bool       -- need sudo?
         → IO String  -- root checker prefix
ifadmin adm =
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  return [] -- TODO: check for administrator on windows
#else
  if adm then fmap (== 0) getRealUserID
              >>= \ isRoot → if isRoot then return []
                             else return "sudo "
         else return []
#endif

-- need for paths with spaces
getMyMsGit :: MyEnv -- environment
            → Bool  -- if needs root
            → IO (String, String)
getMyMsGit myEnv adm = do
    i ← ifadmin adm
    return ( i ⧺ g
           , i ⧺ "\"" ⧺ g ⧺ "\"" )
  where g :: String
        g = git myEnv

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
