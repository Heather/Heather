{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Config
  ( getConfig
  , getDefaultsConfig

  , processChecks
  , processDefaultsChecks

  , withConfig
  , withDefaultsConfig

  , config
  , defaultsConfig

  , getA, getAC
  , getD, getDC

  , enable
  , hashupdate
  , vcsupdate

  , module Yaml
  ) where

import           Exec
import           Yaml

import           Control.Monad.IfElse
import           System.Directory
import           System.Environment            (getEnv)
import           System.Environment.Executable (getExecutablePath)
import           System.FilePath               (takeDirectory, (</>))
import           System.Info                   (os)

import           Data.List

getConfig ∷ IO FilePath
getConfig =
  if | os ∈ ["win32", "mingw32", "cygwin32"] →
        ((</> "sharingan.yml") . takeDirectory <$> getExecutablePath)
     | otherwise → return "/etc/sharingan.yml"

getDefaultsConfig ∷ IO FilePath
getDefaultsConfig =
  if | os ∈ ["win32", "mingw32", "cygwin32"] →
        ((</> "sharinganDefaults.yml") . takeDirectory <$> getExecutablePath)
     | otherwise → return "/etc/sharinganDefaults.yml"

processChecks
  ∷ FilePath --config path
  → IO()
processChecks cfg =
    let generate xcfg = ifNot $ yEncode xcfg nothing
    in doesFileExist cfg ≫= generate cfg
  where nothing = [] ∷ [RepositoryWrapper]

processDefaultsChecks
  ∷ FilePath  --config path
  → IO()
processDefaultsChecks cfg =
    let generate xcfg = ifNot $ yEncode xcfg nothing
    in doesFileExist cfg ≫= generate cfg
  where nothing = DefaultsWrapper
                  (Defaults Nothing Nothing
                            Nothing Nothing)

withConfig ∷ ∀ β. (FilePath → IO β) → IO β
withConfig foo = liftM2 (≫) processChecks foo =≪ getConfig

withDefaultsConfig ∷ ∀ β. (FilePath → IO β) → IO β
withDefaultsConfig foo = liftM2 (≫) processDefaultsChecks foo =≪ getDefaultsConfig

config ∷ IO()
config = do editor ← getEnv "EDITOR"
            withConfig $ \ymlx →
                  exec $ editor ⧺ " " ⧺ ymlx

defaultsConfig ∷ IO ()
defaultsConfig = do editor ← getEnv "EDITOR"
                    withDefaultsConfig $ \ymlx →
                        exec $ editor ⧺ " " ⧺ ymlx

getA ∷ String         -- action for new repository
     → Maybe String   -- new repository group
     → String         -- new repository directory
     → IO ()
getA daction grp arg = -- Add new stuff to sync
  withConfig $ \ymlx →
   whenM <| doesFileExist ymlx <|
     do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
        let rsdata = map _getRepository jsdata
            new = Repository arg daction -- default task / action
                      ["master"] "upstream master"
                      Nothing Nothing Nothing
                      Nothing grp Nothing Nothing
        if new ∈ rsdata
            then putStrLn "this repository is already in sync"
            else yEncode ymlx $ map RepositoryWrapper (new : rsdata)

getD ∷ String -- directory (or part) of repository to remove
     → IO ()
getD arg = -- Remove stuff from sync
  withConfig $ \ymlx →
    whenM <| doesFileExist ymlx <|
      do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
         let rsdata = map _getRepository jsdata
             iio χ = isInfixOf arg $ location χ
             findx = find iio rsdata
         case findx of
            Just fnd → do yEncode ymlx
                            $ map RepositoryWrapper
                                (filter (≠ fnd) rsdata)
                          putStrLn $ location fnd ⧺ " is removed"
            Nothing → putStrLn $ arg ⧺ " repo not found"

getAX ∷ Maybe String → Maybe String → String → IO ()
getAX Nothing  = getA "rebase"
getAX (Just τ) = getA τ

-- add current directory as repository
getAC ∷ Maybe String → Maybe String → Maybe String → IO ()
getAC Nothing  τ η = getCurrentDirectory ≫= getAX τ η
getAC (Just χ) τ η = getAX τ η χ

-- remove current directory from repositories
getDC ∷ [String] → IO ()
getDC []     = getCurrentDirectory ≫= getD
getDC [χ]    = getD χ
getDC (χ:xs) = getD χ >> getDC xs

enable ∷ Bool   -- set enabled/disabled
       → String -- repository path (or part)
       → IO ()
enable en arg =
  withConfig $ \ymlx →
    whenM <| doesFileExist ymlx <|
      do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
         let rsdata = map _getRepository jsdata
             fr χ = if isInfixOf arg $ location χ
                      then χ { enabled = Just en }
                      else χ
         yEncode ymlx $ map (RepositoryWrapper . fr) rsdata

vcsupdate ∷ String -- new vcs
          → String -- repository path (or part)
          → IO ()
vcsupdate vcx rep =
 withConfig $ \ymlx →
   whenM <| doesFileExist ymlx <|
     do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
        let rsdata = map _getRepository jsdata
            fr χ = if rep ≡ location χ
                     then χ { vcs = Just vcx }
                     else χ
        yEncode ymlx $ map (RepositoryWrapper . fr) rsdata

hashupdate ∷ String -- new hash
           → String -- repository path (or part)
           → IO ()
hashupdate hsh rep =
  withConfig $ \ymlx →
    whenM <| doesFileExist ymlx <|
      do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
         let rsdata = map _getRepository jsdata
             fr χ = if rep ≡ location χ
                      then χ { hash = Just hsh }
                      else χ
         yEncode ymlx $ map (RepositoryWrapper . fr) rsdata
