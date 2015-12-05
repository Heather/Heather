{-# LANGUAGE
    MultiWayIf
  , LambdaCase
  , CPP
  , UnicodeSyntax
  #-}

module Shell.Rebase
  ( rebasefork
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

import Shell.Helper

rebasefork :: String → String → [String]
            → Bool → Bool → Bool  → Bool → Maybe String
            → Bool → MyEnv → IO (Bool, Bool)
rebasefork path branch up unsafe frs pC adm rhash pullonly myEnv =
  doesDirectoryExist path ≫= \dirExists →
    if dirExists then execRebaseFork
                 else return (False, False)
  where
    gU :: String -- upstream remote and branch as one string
    gU = unwords up

    gitX :: IO (Bool, Bool)
    gitX = vd ".git" path $ do
      let (myGit, msGit) = getMyMsGit myEnv adm
      currentbranch ← readProcess myGit ["rev-parse", "--abbrev-ref", "HEAD"] []
      let cbr = trim currentbranch
          whe c s = when c $ exec s
      whe (cbr ≢ branch) $ msGit ⧺ " checkout " ⧺ branch
      whe (not unsafe)   $ msGit ⧺ " reset --hard & " ⧺ msGit ⧺ " rebase --abort"
      whe pC             $ msGit ⧺ " clean -xdf"
      localCommit ←
        case rhash of
          Just hsh → return hsh
          _ → readProcess myGit ["log", "-n", "1"
                                , "--pretty=format:%H"
                                ] []
      urlm ← readIfSucc myGit (["ls-remote"] ⧺ up)
      case urlm of
       Nothing → do
         putStrLn "Can't find upstream repository or branch"
         return (False, False)
       Just remt → do
        let remote = head (splitOn "\t" remt)
            local  = trim localCommit
        putStrLn $ "Last Merge: " ⧺ local
        putStrLn $ "Remote: " ⧺ remote
        if remote ≡ local ∧ not frs
          then do putStrLn $ path ⧺ " is up to date"
                  return (True, False)
          else do
            rlm ← readIfSucc myGit ["ls-remote", "origin", branch]
            case rlm of
             Just rlc → do
               lrc ← if isNothing rhash
                 then readProcess myGit ["log", "-n", "1"
                                        , "--pretty=format:%H"
                                        ] []
                 else return local
               let remloc = head (splitOn "\t" rlc)
                   locloc = trim lrc
               putStrLn $ "Origin: " ⧺ remloc
               putStrLn $ "Local: "  ⧺ locloc
               whe (remloc ≢ locloc ∨ frs) $ msGit ⧺ " pull origin " ⧺ branch
               when (remloc ≢ remote ∨ frs)
                 $ do exec $ msGit ⧺ " pull --rebase " ⧺ gU
                      unless pullonly
                        $ do exec $ msGit
                                 ⧺ " push --force origin "
                                 ⧺ branch
                             hashupdate remote path
               return (True, True)
             _ → return (False, False)

    hgX :: IO (Bool, Bool)
    hgX = vd ".hg" path $ do
      exec $ "hg pull --update --rebase" ⧺ gU
      unless pullonly
        $ exec $ "hg push " ⧺ branch ⧺ " --force"
      return (True, True)

    execRebaseFork :: IO (Bool, Bool)
    execRebaseFork = return (False, False) ≫= chk gitX
                                           ≫= chk hgX
