{-# LANGUAGE
    UnicodeSyntax
  , LambdaCase
  , CPP
  #-}

module EnvChecker
  ( getEnv
  , version
  ) where

import Yaml
import Exec

import Data.Maybe

import Paths_Sharingan (version)

#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
#else
-- needs posix in cabal
-- import System.Posix.User
#endif

isRoot :: IO Bool
isRoot =
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  return False
#else
  return True
  -- fmap (== 0) getRealUserID
#endif

checkIfSucc :: String → [String] → IO (Maybe String)
checkIfSucc cmd args =
  readCheck cmd args
  ≫= \case Left _ → return Nothing
           Right val → do putStr $ cmd ⧺ " : " ⧺ val
                          return (Just cmd)

gitCheck :: String → [String] → IO (Maybe String)
gitCheck cmd args = checkIfSucc cmd (args ++ ["--version"])

getGit :: IO String
getGit = return Nothing ≫= t "git" []
                        ≫= t "C:/Program Files (x86)/Git/cmd/git.exe" []
                        ≫= t "C:/Program Files/Git/cmd/git.exe" []
                        ≫= t "git.cmd" []
                        ≫= \res → return $ fromMaybe "git" res
  where t :: String → [String] → Maybe String → IO (Maybe String)
        t x a prev = if isNothing  prev
                      then gitCheck x a
                      else return prev

getEnv :: IO MyEnv
getEnv = do
  myGit ← getGit
  return (MyEnv myGit "hg")
