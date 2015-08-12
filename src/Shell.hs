{-# LANGUAGE
    MultiWayIf
  , LambdaCase
  , CPP
  , UnicodeSyntax
  #-}

module Shell
  ( amaterasu
  , setEnv
  , module Exec
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

amaterasu :: String → String → String → [String]
           → Bool → Bool → Maybe String
           → MyEnv → IO (Bool, Bool)
amaterasu "rebase"  = rebasefork
amaterasu "pull"    = pull
amaterasu custom    = \path _ _ _ _ _ _ →
  doesDirectoryExist path ≫= \dirExist →
    if dirExist then setCurrentDirectory path ≫ do
                        exec custom
                        return (True, True)
                else return (False, False)

pull :: String → String → [String]
      → Bool → Bool → Maybe String
      → MyEnv → IO (Bool, Bool)
pull path branch _ unsafe processClean rhash myEnv =
    doesDirectoryExist path ≫= \dirExists →
      if dirExists then execPull
                   else return (False, False)
  where
    gitX :: IO (Bool, Bool)
    gitX = vd ".git" path $ do
      let (myGit, msGit) = getMyMsGit myEnv
          whe c s = when c $ exec s
      currentbranch ← readProcess myGit ["rev-parse", "--abbrev-ref", "HEAD"] []
      let cbr = trim currentbranch
      whe (cbr ≢ branch) $ msGit ⧺ " checkout " ⧺ branch
      whe (not unsafe)   $ msGit ⧺ " reset --hard"
      whe processClean   $ msGit ⧺ " clean -xdf"
      loc ← case rhash of
              Just hsh → return hsh
              _ → readProcess myGit ["log", "-n", "1"
                                    , "--pretty=format:%H"
                                    ] []
      rlm ← readIfSucc myGit ["ls-remote", "origin", branch]
      case rlm of
       Just rlc → do
         lrc ← if isNothing rhash
           then readProcess myGit ["log", "-n", "1"
                                  , "--pretty=format:%H"
                                  ] []
           else return (trim loc)
         let remoteloc = head (splitOn "\t" rlc)
             localloc = trim lrc
         putStrLn $ "Origin: " ⧺ remoteloc
         putStrLn $ "Local: "  ⧺ localloc
         if remoteloc ≢ localloc
             then do exec $ msGit ⧺ " pull origin " ⧺ branch
                     hashupdate remoteloc path
                     return (True, True)
             else return (True, False)
       _ → return (False, False)

    hgX :: IO (Bool, Bool)
    hgX = vd ".hg" path $ do
      exec "hg pull --update"
      return (True, True)

    execPull :: IO (Bool, Bool)
    execPull = return (False, False) ≫= chk gitX
                                     ≫= chk hgX

rebasefork :: String → String → [String]
            → Bool → Bool → Maybe String
            → MyEnv → IO (Bool, Bool)
rebasefork path branch up unsafe pC rhash myEnv =
  doesDirectoryExist path ≫= \dirExists →
    if dirExists then execRebaseFork
                 else return (False, False)
  where
    gU :: String -- upstream remote and branch as one string
    gU = unwords up

    gitX :: IO (Bool, Bool)
    gitX = vd ".git" path $ do
      let (myGit, msGit) = getMyMsGit myEnv
      currentbranch ← readProcess myGit ["rev-parse", "--abbrev-ref", "HEAD"] []
      let cbr = trim currentbranch
          whe c s = when c $ exec s
      whe (cbr ≢ branch) $ msGit ⧺ " checkout " ⧺ branch
      whe (not unsafe)   $ msGit ⧺ " reset --hard & " ⧺ msGit ⧺ " rebase --abort"
      whe pC             $ msGit ⧺ " clean -xdf"
      localCommit ←
        case rhash of
          Just hsh → return hsh
          _ → if length up > 1
                then readProcess myGit [ "rev-parse"
                                       , intercalate "/" up
                                       ] []
                else readProcess myGit ["log", "-n", "1"
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
        if remote ≡ local
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
               whe (remloc ≢ locloc) $ msGit ⧺ " pull origin " ⧺ branch
               whe (remloc ≢ remote)
                     $ msGit ⧺ " pull --rebase " ⧺ gU
                             ⧺ " & " ⧺ msGit
                             ⧺ " push --force origin " ⧺ branch
               hashupdate remote path
               return (True, True)
             _ → return (False, False)

    hgX :: IO (Bool, Bool)
    hgX = vd ".hg" path $ do
      exec $ "hg pull --update --rebase" ⧺ gU
          ⧺ " & hg push " ⧺ branch
          ⧺ " --force"
      return (True, True)

    execRebaseFork :: IO (Bool, Bool)
    execRebaseFork = return (False, False) ≫= chk gitX
                                           ≫= chk hgX
