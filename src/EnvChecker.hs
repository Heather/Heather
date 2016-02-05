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

checkIfSucc           -- check if success
 :: String            -- command to check
  → [String]          -- arguments
  → IO (Maybe String) -- Just cmd in case of success
checkIfSucc cmd args =
  readCheck cmd args
  ≫= \case Left _ → return Nothing
           Right val → do putStr $ cmd ⧺ " : " ⧺ val
                          return (Just cmd)

versionCheck          -- check for git --version
 :: String            -- command to check
  → [String]          -- arguments
  → IO (Maybe String) -- Path to git in case of success
versionCheck cmd args = checkIfSucc cmd (args ++ ["--version"])

getGit :: IO String
getGit = return Nothing ≫= t "git" []
                        ≫= t "C:/Program Files (x86)/Git/cmd/git.exe" []
                        ≫= t "C:/Program Files/Git/cmd/git.exe" []
                        ≫= t "git.cmd" []
                        ≫= \res → return $ fromMaybe "git" res
  where t :: String → [String] → Maybe String → IO (Maybe String)
        t x a prev = if isNothing  prev
                      then versionCheck x a
                      else return prev

getHg :: IO String
getHg = return Nothing ≫= t "hg" []
                       ≫= t "hg.cmd" []
                       ≫= \res → return $ fromMaybe "hg" res
  where t :: String → [String] → Maybe String → IO (Maybe String)
        t x a prev = if isNothing  prev
                      then versionCheck x a
                      else return prev

getEnv :: IO MyEnv
getEnv = do
  myGit ← getGit
  myHg  ← getHg
  return (MyEnv myGit myHg)
