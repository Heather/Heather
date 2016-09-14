{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Sync
  ( synchronize
  , list
  , status
  ) where

import           Config
import           Despair
import           EnvChecker
import           SharinganProcess
import           Tools

import           Text.Printf

import           System.Directory
import           System.Exit
import           System.FilePath   ((</>))
import           System.Info       (os)
import           System.IO

import           Data.Char         (toLower)
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Version      (showVersion)

import           Control.Exception

synchronize     -- actual synchronization function
  ∷ CommonOpts  -- common options
  → SyncOpts    -- synchronization options
  → IO()
synchronize _o σ = -- ( ◜ ①‿‿① )◜
  withDefaultsConfig $ \defx →
   withConfig $ \ymlx → despair $ do
    jsdat ← yDecode ymlx ∷ IO [RepositoryWrapper]
    jfdat ← yDecode defx ∷ IO DefaultsWrapper
    myenv ← getEnv
    let dfdat = _getDefaults jfdat
        rsdat = map _getRepository jsdat
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
    when (syncFull σ ∨ (fromMaybe False (full dfdat))) $ do
      when (fromMaybe True (updateCabal dfdat)) cabalUpdate
      when (fromMaybe False (updateStack dfdat)) stackUpdate
#endif
    forM_ rsdat $ sync myenv dfdat where
  sync ∷ MyEnv       -- environment
        → Defaults    -- default options
        → Repository  -- repository (iterating)
        → IO ()
  sync myEnv dfdata repo =
    let loc = location repo
        isenabled = fromMaybe True (enabled repo)
        frs = syncForce σ
        ntr = syncInteractive σ
        nps = syncNoPush σ
    in when (case syncFilter σ of
                    Nothing  → case syncGroups σ of
                                    [] → isenabled
                                    θ  → case syncGroup repo of
                                                Just γ → isenabled ∧ (γ ∈ θ)
                                                Nothing → False
                    Just snc → isInfixOf <| map toLower snc
                                         <| map toLower loc)
      $ let ups = splitOn " " $ upstream repo
            snc = sharingan (syncInteractive σ)
            cln = fromMaybe False (clean repo)
            noq = not $ fromMaybe False (quick dfdata)
            tsk = task repo
            vcx = vcs repo
            λ b = do
              printf " - %s : %s\n" loc b
              if nps ∧ tsk /= "pull"
                then return (True, True)
                else amaterasu tsk loc b ups (syncUnsafe σ)
                        frs cln (hash repo) myEnv vcx
            eye (_, ρ) = when ((ρ ∨ frs) ∧ not (syncQuick σ) ∧ noq)
              $ do let shx = loc </> ".sharingan.yml"
                       ps  = postRebuild repo
                   doesFileExist shx ≫= sharingan
                                          ntr shx loc
                   when (isJust ps) $ forM_ (fromJust ps) $ \psc →
                      let pshx = psc </> ".sharingan.yml"
                      in doesFileExist pshx≫= snc pshx psc
        in do forM_ (tails (branches repo))
               $ \case [χ] → λ χ ≫= eye -- Tail
                       χ:_ → λ χ ≫= (\_ → return ())
                       []  → return ()
              putStrLn ⊲ replicate 80 '_'

iterateConfig ∷ [String] → (Int → Repository → IO ()) → IO()
iterateConfig xs action = withConfig $ \ymlx → do
  jsdat ← yDecode ymlx ∷ IO [RepositoryWrapper]
  let rsdata = map _getRepository jsdat
      rdd = case xs of [] → rsdata
                       _  → filter (isInfixOf (head xs) . location) rsdata
      maxl = maximum $ map (length . last . splitOn "\\" . location) rdd
  forM_ rdd (action maxl)

list ∷ [String] → IO() -- (＾‿‿＾ *)
list xs = iterateConfig xs $ \maxl repo →
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
status xs = iterateConfig xs $ \maxl repo →
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
