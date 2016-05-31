{-# LANGUAGE CPP           #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE UnicodeSyntax #-}

module EnvChecker
  ( getEnv
  , version
  ) where

import           Exec
import           Yaml

import           Data.Maybe

import           Paths_Sharingan (version)

checkIfSucc           -- check if success
 ∷ String            -- command to check
  → [String]          -- arguments
  → IO (Maybe String) -- Just cmd in case of success
checkIfSucc γ args =
  readCheck γ args
  ≫= \case Left _ → return Nothing
           Right val → do putStr $ γ ⧺ " : " ⧺ val
                          return (Just γ)

versionCheck          -- check for git --version
 ∷ String            -- command to check
  → IO (Maybe String) -- Path to git in case of success
versionCheck γ = checkIfSucc γ ["--version"]

getGit ∷ IO String
getGit = return Nothing ≫= λ "git"
                        ≫= λ "C:/Program Files (x86)/Git/cmd/git.exe"
                        ≫= λ "C:/Program Files/Git/cmd/git.exe"
                        ≫= λ "git.cmd"
                        ≫= \res → return $ fromMaybe "git" res
  where λ ∷ String → Maybe String → IO (Maybe String)
        λ χ prev = if isNothing  prev
                      then versionCheck χ
                      else return prev

getHg ∷ IO String
getHg = return Nothing ≫= λ "hg"
                       ≫= λ "hg.cmd"
                       ≫= \res → return $ fromMaybe "hg" res
  where λ ∷ String → Maybe String → IO (Maybe String)
        λ χ prev = if isNothing  prev
                      then versionCheck χ
                      else return prev

getEnv ∷ IO MyEnv
getEnv = do
  myGit ← getGit
  myHg  ← getHg
  return (MyEnv myGit myHg)
