{-# LANGUAGE UnicodeSyntax #-}

module EnvChecker
  ( getEnv
  , readIfSucc
  ) where

import Yaml
import Exec

import Control.Monad
import Control.Eternal

import Data.Maybe

gitCheck :: String → [String] → IO (Maybe String)
gitCheck cmd args = readIfSucc cmd (args ++ ["--version"])

getGit :: IO String
getGit = (return Nothing) ≫= t "git" []
                          ≫= t "C:/Program Files (x86)/Git/cmd/git.exe" []
                          ≫= t "C:/Program Files/Git/cmd/git.exe" []
                          ≫= t "git.cmd" []
                          ≫= \res → return $ fromMaybe "git" res
  where t x a prev = if (isNothing  prev)
                      then gitCheck x a
                      else return prev

getEnv :: IO MyEnv
getEnv = do
  myGit ← getGit
  return (MyEnv myGit "hg")
