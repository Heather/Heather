{-# LANGUAGE CPP           #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Shell.Rebase
  ( rebasefork
  ) where

import           Data.List
import           Data.List.Split
import           Data.Maybe

import           System.Directory
import           System.FilePath  ((</>))
import           System.Info      (os)

import           System.Process

import           Config
import           Exec
import           Trim

import           Shell.Helper

rebasefork
  ∷ String           -- location
   → String           -- branch
   → [String]         -- splitted upstream (splitOn " " $ upstream repo)
   → Bool             -- unsafe
   → Bool             -- force
   → Bool             -- clean
   → Maybe String     -- Hash
   → MyEnv            -- environment
   → Maybe String     -- VCS
   → IO (Bool, Bool)  -- success & continue
rebasefork path branch up unsafe frs pC rhash myEnv vcx =
  doesDirectoryExist path ≫= \dirExists →
    if dirExists then execRebaseFork
                 else return (False, False)
  where
    remoteSpaceBranch ∷ String -- upstream remote and branch as one string
    remoteSpaceBranch = unwords up

    gitRebase ∷ IO (Bool, Bool)
    gitRebase = vd ".git" vcx path $ do
      let (myGit, msGit) = getMyMsGit myEnv
      currentbranch  ← readProcess myGit ["rev-parse", "--abbrev-ref", "HEAD"] []
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
                 $ do exec $ msGit ⧺ " pull --rebase " ⧺ remoteSpaceBranch
                      exec $ msGit ⧺ " push --force origin "
                                   ⧺ branch
                      hashupdate remote path
               return (True, True)
             _ → return (False, False)

    hgRebase ∷ IO (Bool, Bool)
    hgRebase = vd ".hg" vcx path $ do
      exec $ "hg pull --update --rebase" ⧺ remoteSpaceBranch
      exec $ "hg push " ⧺ branch ⧺ " --force"
      return (True, True)

    execRebaseFork ∷ IO (Bool, Bool)
    execRebaseFork = return (False, False) ≫= chk gitRebase
                                           ≫= chk hgRebase
