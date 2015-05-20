{-# LANGUAGE MultiWayIf, LambdaCase, CPP, UnicodeSyntax #-}

module Shell
  ( amaterasu
  , setEnv
#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  , gentooSync
#endif
  , module Exec
  ) where

import Data.List.Split

import System.IO
import System.Info (os)
import System.Directory
import System.FilePath((</>))

import System.Process

import Trim
import Exec
import Config
import AsyncReactive

setEnv :: String → IO()
setEnv env = sys $ if | os ∈ ["win32", "mingw32"] → "set " ⧺ env
                      | os ∈ ["darwin", "cygwin32"] → "export " ⧺ env
                      | otherwise → "export " ⧺ env

--vd :: String → String → IO (Bool, Bool)
vd validator path action =
  doesDirectoryExist ⊲ path </> validator ≫= \vde →
          if vde then setCurrentDirectory path ≫ action
                 else return (True, False)

amaterasu :: String → String → String → [String] → Bool → Bool → Maybe String → Bool → MyEnv → IO (Bool, Bool)
amaterasu "rebase"  = rebasefork
amaterasu "pull"    = pull
amaterasu custom    = \path _ _ unsafe processClean _ _ myEnv →
  doesDirectoryExist path ≫= \dirExist →
    if dirExist then setCurrentDirectory path ≫ do
                        exec custom
                        return (True, True)
                else return (False, False)

pull :: String → String → [String] → Bool → Bool → Maybe String → Bool → MyEnv → IO (Bool, Bool)
pull path branch _ unsafe processClean rhash sync myEnv =
    doesDirectoryExist path ≫= \dirExists →
      if not dirExists then return (False, False)
                       else execPull path branch unsafe processClean rhash sync myEnv
  where
    execPull :: String → String → Bool → Bool → Maybe String → Bool → MyEnv → IO (Bool, Bool)
    execPull path branch unsafe processClean rhash sync myEnv =
          let chk foo (previous,doU) = if previous
                  then return (previous, doU)
                  else foo

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

              hgX = vd ".hg" path $ do
                exec $ "hg pull --update"
                return (True, True)

          in (return (False, False)) ≫= (chk gitX)
                                     ≫= (chk hgX)

rebasefork :: String → String → [String] → Bool → Bool → Maybe String → Bool → MyEnv → IO (Bool, Bool)
rebasefork path branch up unsafe processClean rhash sync myEnv =
  doesDirectoryExist path ≫= \dirExists →
    if not dirExists then return (False, False)
                     else execRebaseFork path branch up unsafe processClean rhash sync myEnv getUpstream
  where
    getUpstream :: String
    getUpstream = intercalate " " up

    execRebaseFork :: String → String → [String] → Bool → Bool → Maybe String → Bool → MyEnv → String → IO (Bool, Bool)
    execRebaseFork path branch up unsafe processClean rhash sync myEnv upstream =
          let chk foo (previous,doU) = if previous
                  then return (previous, doU)
                  else foo

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
                rem ← readProcess myGit (["ls-remote"] ⧺ up) []
                let remote = (splitOn "\t" rem) !! 0
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
                               $ msGit ⧺ " pull --rebase " ⧺ upstream
                                       ⧺ " & " ⧺ msGit
                                       ⧺ " push --force origin " ⧺ branch
                         hashupdate remote path
                         return (True, True)
                       _ → return (False, False)

              hgX = vd ".hg" path $ do
                exec $ "hg pull --update --rebase" ⧺ upstream
                    ⧺ " & hg push " ⧺ branch
                    ⧺ " --force"
                return (True, True)

          in (return (False, False)) ≫= (chk gitX)
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
