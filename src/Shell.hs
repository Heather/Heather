{-# LANGUAGE MultiWayIf
  , LambdaCase
  , CPP
  , UnicodeSyntax #-}

module Shell
  ( amaterasu
  , setEnv
#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  , gentooSync
#endif
  , module Exec
  ) where

import Data.List.Split

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
           → Bool → MyEnv → IO (Bool, Bool)
amaterasu "rebase"  = rebasefork
amaterasu "pull"    = pull
amaterasu custom    = \path _ _ _ _ _ _ _ →
  doesDirectoryExist path ≫= \dirExist →
    if dirExist then setCurrentDirectory path ≫ do
                        exec custom
                        return (True, True)
                else return (False, False)

pull :: String → String → [String]
      → Bool → Bool → Maybe String
      → Bool → MyEnv → IO (Bool, Bool)
pull path branch _ unsafe processClean rhash _ myEnv =
    doesDirectoryExist path ≫= \dirExists →
      if dirExists then execPull
                   else return (False, False)
  where
    gitX :: IO (Bool, Bool)
    gitX = vd ".git" path $ do
      let myGit = git myEnv
          msGit = "\"" ⧺ myGit ⧺ "\""
      currentbranch ← readProcess myGit ["rev-parse", "--abbrev-ref", "HEAD"] []
      let cbr = trim currentbranch
          whe c s = when c $ exec s
      whe (cbr ≢ branch)  $ msGit ⧺ " checkout " ⧺ branch
      whe (not unsafe)    $ msGit ⧺ " reset --hard & " ⧺ msGit ⧺ " rebase --abort"
      whe (processClean)  $ msGit ⧺ " clean -xdf"
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
         let remloc = (splitOn "\t" rlc) !! 0
             locloc = trim lrc
         putStrLn $ "Origin: " ⧺ remloc
         putStrLn $ "Local: "  ⧺ locloc
         if (remloc ≢ locloc)
             then do exec $ msGit ⧺ " pull origin " ⧺ branch
                     hashupdate remloc path
                     return (True, True)
             else return (True, False)
       _ → return (False, False)

    hgX :: IO (Bool, Bool)
    hgX = vd ".hg" path $ do
      exec $ "hg pull --update"
      return (True, True)

    execPull :: IO (Bool, Bool)
    execPull = (return (False, False)) ≫= (chk gitX)
                                       ≫= (chk hgX)

rebasefork :: String → String → [String]
            → Bool → Bool → Maybe String
            → Bool → MyEnv → IO (Bool, Bool)
rebasefork path branch up unsafe pC rhash sync myEnv =
  doesDirectoryExist path ≫= \dirExists →
    if dirExists then execRebaseFork
                 else return (False, False)
  where
    gU :: String -- upstream remote and branch as one string
    gU = intercalate " " up

    gitX :: IO (Bool, Bool)
    gitX = vd ".git" path $ do
      let myGit = git myEnv
          msGit = "\"" ⧺ myGit ⧺ "\""
      currentbranch ← readProcess myGit ["rev-parse", "--abbrev-ref", "HEAD"] []
      let cbr = trim currentbranch
          whe c s = when c $ exec s
      whe (cbr ≢ branch)  $ msGit ⧺ " checkout " ⧺ branch
      whe (not unsafe)    $ msGit ⧺ " reset --hard & " ⧺ msGit ⧺ " rebase --abort"
      whe (pC)            $ msGit ⧺ " clean -xdf"
      loc ← case rhash of
              Just hsh → return hsh
              _ → if (length up) > 1
                      then if sync then readProcess myGit [ "merge-base"
                                                          , up !! 1
                                                          , "origin/" ⧺ branch
                                                          ] []
                                   else readProcess myGit [ "rev-parse"
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
        let remote = (splitOn "\t" remt) !! 0
            local  = trim loc
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
               let remloc = (splitOn "\t" rlc) !! 0
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
    execRebaseFork = (return (False, False)) ≫= (chk gitX)
                                             ≫= (chk hgX)

#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
gentooSync :: String → Int → IO()
gentooSync path jobs = do
    j ← if jobs ≡ 0 then readProcess "nproc" [] []
                      else return $ show jobs
    putStrLn "updating..."
    asyncReactive (exc path $ " cvs update "
                    ⧺ " & egencache --update --repo=gentoo --portdir=" ⧺ path
                    ⧺ " --jobs=" ⧺ j)
#endif
