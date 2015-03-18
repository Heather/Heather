{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

module EnvChecker
  ( getEnv
  ) where

import Yaml

import System.Process
import Control.Monad
import Control.Eternal
import Control.Exception

import Data.Maybe

gitCheckTry :: String → [String] → IO (Either SomeException String)
gitCheckTry cmd args =
    try $ readProcess cmd (args ++ ["--version"]) []
            :: IO (Either SomeException String)

gitCheck :: String → [String] → IO (Maybe String)
gitCheck cmd args =
    gitCheckTry cmd args
    ≫= \case Left _ → return Nothing
             Right val → do putStr $ cmd ++ " : "
                            putStrLn val
                            return (Just cmd)

getGit :: IO String
getGit =
    let t x a prev = if (isNothing  prev)
                      then gitCheck x a
                      else return prev
    in (return Nothing) ≫= t "git" []
                        ≫= t "C:/Program Files (x86)/Git/cmd/git.exe" []
                        ≫= t "C:/Program Files/Git/cmd/git.exe" []
                        ≫= t "git.cmd" []
                        ≫= \res → return $ fromMaybe "git" res

getEnv :: IO MyEnv
getEnv = do
    myGit ← getGit
    return (MyEnv myGit "hg")
