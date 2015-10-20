{-# LANGUAGE
    MultiWayIf
  , UnicodeSyntax
  , RankNTypes
  #-}

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

  , module Yaml
  ) where

import Yaml
import Exec

import System.Directory
import System.Info (os)
import System.Environment.Executable ( getExecutablePath )
import System.FilePath(takeDirectory, (</>))
import System.Environment( getEnv )
import Control.Monad.IfElse

import Data.List

getConfig ∷ IO FilePath
getConfig =
  if | os ∈ ["win32", "mingw32", "cygwin32"] → (</> "sharingan.yml")
                                                <$> takeDirectory
                                                <$> getExecutablePath
     | otherwise → return "/etc/sharingan.yml"

getDefaultsConfig ∷ IO FilePath
getDefaultsConfig =
  if | os ∈ ["win32", "mingw32", "cygwin32"] → (</> "sharinganDefaults.yml")
                                                <$> takeDirectory
                                                <$> getExecutablePath
     | otherwise → return "/etc/sharinganDefaults.yml"

processChecks ∷ FilePath → IO()
processChecks cfg =
    let generate xcfg = ifNot $ yEncode xcfg nothing
    in doesFileExist cfg ≫= generate cfg
  where nothing = [] ∷ [RepositoryWrapper]

processDefaultsChecks ∷ FilePath → IO()
processDefaultsChecks cfg =
    let generate xcfg = ifNot $ yEncode xcfg nothing
    in doesFileExist cfg ≫= generate cfg
  where nothing = DefaultsWrapper (Defaults Nothing)

withConfig :: ∀ β. (FilePath → IO β) → IO β
withConfig foo = liftM2 (≫) processChecks foo =≪ getConfig

withDefaultsConfig :: ∀ β. (FilePath → IO β) → IO β
withDefaultsConfig foo = liftM2 (≫) processDefaultsChecks foo =≪ getDefaultsConfig

config ∷ IO()
config = do editor ← getEnv "EDITOR"
            withConfig $ \ymlx →
                  exec $ editor ⧺ " " ⧺ ymlx

defaultsConfig ∷ IO ()
defaultsConfig = do editor ← getEnv "EDITOR"
                    withDefaultsConfig $ \ymlx →
                        exec $ editor ⧺ " " ⧺ ymlx

getA ∷ String → Maybe String → String → IO ()
getA daction grp arg = -- Add new stuff to sync
  withConfig $ \ymlx →
   whenM <| doesFileExist ymlx <|
     do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
        let rsdata = map _getRepository jsdata
            new = Repository arg daction -- default task / action
                      ["master"] "upstream master"
                      Nothing Nothing Nothing
                      Nothing Nothing grp Nothing
        if new ∈ rsdata
            then putStrLn "this repository is already in sync"
            else yEncode ymlx $ map RepositoryWrapper (new : rsdata)

getD ∷ String → IO ()
getD arg = -- Remove stuff from sync
  withConfig $ \ymlx →
    whenM <| doesFileExist ymlx <|
      do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
         let rsdata = map _getRepository jsdata
             iio x = isInfixOf arg $ location x
             findx = find iio rsdata
         case findx of
            Just fnd → do yEncode ymlx
                            $ map RepositoryWrapper
                                (filter (≠ fnd) rsdata)
                          putStrLn $ location fnd ⧺ " is removed"
            Nothing → putStrLn $ arg ⧺ " repo not found"

getAX ∷ Maybe String → Maybe String → String → IO ()
getAX Nothing   = getA "rebase"
getAX (Just t)  = getA t

getAC ∷ Maybe String → Maybe String → Maybe String → IO ()
getAC Nothing  t g  = getCurrentDirectory ≫= (getAX t g)
getAC (Just x) t g  = getAX t g x

getDC ∷ [String] → IO ()
getDC []     = getCurrentDirectory ≫= getD
getDC [x]    = getD x
getDC (x:xs) = getD x >> getDC xs

enable ∷ Bool → String → IO ()
enable en arg =
  withConfig $ \ymlx →
    whenM <| doesFileExist ymlx <|
      do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
         let rsdata = map _getRepository jsdata
             fr x = if isInfixOf arg $ location x
                      then x { enabled = Just en }
                      else x
         yEncode ymlx $ map (RepositoryWrapper . fr) rsdata

hashupdate ∷ String → String → IO ()
hashupdate hsh rep =
  withConfig $ \ymlx →
    whenM <| doesFileExist ymlx <|
      do jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
         let rsdata = map _getRepository jsdata
             fr x = if rep ≡ location x
                      then x { hash = Just hsh }
                      else x
         yEncode ymlx $ map (RepositoryWrapper . fr) rsdata
