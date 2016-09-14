{-# LANGUAGE CPP           #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Shell.Pull
  ( pull
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

pull
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
pull path branch _ unsafe frs processClean rhash myEnv vcx =
    doesDirectoryExist path ≫= \dirExists →
      if dirExists then execPull
                   else return (False, False)
  where
    gitPull ∷ IO (Bool, Bool)
    gitPull = vd ".git" vcx path $ do
      let (myGit, msGit) = getMyMsGit myEnv
      currentbranch  ← readProcess myGit ["rev-parse", "--abbrev-ref", "HEAD"] []
      let cbr = trim currentbranch
          whe c s = when c $ exec s
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
         if remoteloc ≢ localloc ∨ frs
             then do exec $ msGit ⧺ " pull origin " ⧺ branch
                     hashupdate remoteloc path
                     return (True, True)
             else return (True, False)
       _ → return (False, False)

    hgPull ∷ IO (Bool, Bool)
    hgPull = vd ".hg" vcx path $ do
      exec "hg pull --update"
      return (True, True)

    execPull ∷ IO (Bool, Bool)
    execPull = return (False, False) ≫= chk gitPull
                                     ≫= chk hgPull
