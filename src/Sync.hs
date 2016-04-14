{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
  , CPP
  , UnicodeSyntax
  #-}

module Sync
  ( synchronize
  , list
  , status
  ) where

import Despair
import EnvChecker
import Tools
import Config
import SharinganProcess

import Text.Printf

import System.Info (os)
import System.Directory
import System.Exit
import System.IO
import System.FilePath ((</>))

import Data.Char (toLower)
import Data.List
import Data.Maybe
import Data.List.Split
import Data.Version (showVersion)

import Control.Exception

synchronize     -- actual synchronization function
  ∷ CommonOpts  -- common options
  → SyncOpts    -- synchronization options
  → IO()
synchronize _o so = -- ( ◜ ①‿‿① )◜
  withDefaultsConfig $ \defx →
   withConfig $ \ymlx → despair $ do
    jsdat ← yDecode ymlx ∷ IO [RepositoryWrapper]
    jfdat ← yDecode defx ∷ IO DefaultsWrapper
    myenv ← getEnv
    let dfdat = _getDefaults jfdat
        rsdat = map _getRepository jsdat
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
    when (syncFull so ∨ (fromMaybe False (full dfdat))) $ do
      when (fromMaybe True (updateCabal dfdat)) cabalUpdate
      when (fromMaybe False (updateStack dfdat)) stackUpdate
#endif
    forM_ rsdat $ sync myenv dfdat where
  sync :: MyEnv       -- environment
        → Defaults    -- default options
        → Repository  -- repository (iterating)
        → IO ()
  sync myEnv dfdata repo =
    let loc = location repo
        isenabled = fromMaybe True (enabled repo)
        frs = syncForce so
        ntr = syncInteractive so
        nps = syncNoPush so
    in when (case syncFilter so of
                    Nothing  → case syncGroups so of
                                    [] → isenabled
                                    gx  → case syncGroup repo of
                                                Just gg → isenabled ∧ (gg ∈ gx)
                                                Nothing → False
                    Just snc → isInfixOf <| map toLower snc
                                         <| map toLower loc)
      $ let ups = splitOn " " $ upstream repo
            snc = sharingan (syncInteractive so) adm
            cln = fromMaybe False (clean repo)
            adm = fromMaybe False (root repo)
            noq = not $ fromMaybe False (quick dfdata)
            tsk = task repo
            vcx = vcs repo
            u b = do
              printf " - %s : %s\n" loc b
              if nps ∧ tsk /= "pull"
                then return (True, True)
                else amaterasu tsk loc b ups (syncUnsafe so)
                        frs cln adm (hash repo) myEnv vcx
            eye (_, r) = when ((r ∨ frs) ∧ not (syncQuick so) ∧ noq)
              $ do let shx = loc </> ".sharingan.yml"
                       ps  = postRebuild repo
                   doesFileExist shx ≫= sharingan
                        ntr adm shx loc
                   when (isJust ps) $ forM_ (fromJust ps) $ \psc →
                      let pshx = psc </> ".sharingan.yml"
                      in doesFileExist pshx≫= snc pshx psc
        in do forM_ (tails (branches repo))
               $ \case [x] → u x ≫= eye -- Tail
                       x:_ → u x ≫= (\_ → return ())
                       []  → return ()
              putStrLn ⊲ replicate 80 '_'

list ∷ [String] → IO() -- (＾‿‿＾ *)
list xs = withConfig $ \ymlx → do
  jsdat ← yDecode ymlx ∷ IO [RepositoryWrapper]
  let rsdata = map _getRepository jsdat
      rdd = case xs of [] → rsdata
                       _  → filter (isInfixOf (head xs) . location) rsdata
      maxl = maximum $ map (length . last . splitOn "\\" . location) rdd
  forM_ rdd $ \repo →
     let locs = location repo
         name = last $ splitOn "\\" locs
         lnam = maxl + 1 - length name
         sstr = " - " ⧺ name ⧺ if lnam > 0 then replicate lnam ' '
                                           else ""
         empt = replicate (length sstr) ' '
         brx  = branches repo
     in if length brx ≡ 0
         then printf " - %s\n" locs
         else do printf "%s: %s (%s) %s" sstr (head brx) locs (task repo)
                 unless (fromMaybe True (enabled repo))
                   $ putStr " [Disabled]"
                 putStrLn ""
                 forM_ (drop 1 brx) $ printf "%s: %s\n" empt

status ∷ [String] → IO() -- (＾‿‿＾ *)
status xs = withConfig $ \ymlx → do
  jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
  let rsdata = map _getRepository jsdata
      rdd = case xs of [] → rsdata
                       _  → filter (isInfixOf (head xs) . location) rsdata
      maxl = maximum $ map (length . last . splitOn "\\" . location) rdd
  forM_ rdd $ \repo →
     let loc  = location repo
         name = last $ splitOn "\\" loc
         lnam = maxl + 1 - length name
         sstr = " - " ⧺ name ⧺ if lnam > 0 then replicate lnam ' '
                                           else ""
         stat = case positive repo of
                      Just True  → "OK" ∷ String
                      Just False → "Errors..."
                      Nothing    → "?"
     in printf "%s- %s\n" sstr stat
