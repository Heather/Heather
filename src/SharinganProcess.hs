{-# LANGUAGE
    CPP
  , MultiWayIf
  , UnicodeSyntax
  #-}

module SharinganProcess
  ( sharingan
  , amaterasu
  ) where

import Yaml
import Config
import Shell

import System.Directory
import System.FilePath ((</>))
import System.Exit

import Data.Maybe
import Data.List
import Data.Char

updateStatusIcon ∷ String → Bool → IO()
updateStatusIcon loc pos =
  withConfig $ \ymlx →
    let ymlprocess = ifSo $ do
        jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
        let rsdata = map _getRepository jsdata
            fr x = if isInfixOf loc $ location x
                    then x { positive = Just pos }
                    else x
        yEncode ymlx $ map (RepositoryWrapper . fr) rsdata
    in doesFileExist ymlx ≫= ymlprocess

sharingan ∷ Bool → String → String → Bool → IO()
sharingan interactive shx loc shxi = if shxi then
   do jsyncDatax ← yDecode shx ∷ IO SharinganWrapper
      let syncDatax = _getSharingan jsyncDatax
          sc   = script syncDatax
          lang = case language syncDatax of
                      Just [] → []
                      Just ln → map toLower ln
                      _ → []
      forM_ (fromMaybe [] (env syncDatax)) setEnv
      forM_ (fromMaybe [] (beforeInstall syncDatax)) exth
      case install syncDatax of
        Just []  → return () -- do nothing
        Just ilX → forM_ ilX exth
        _ → case lang of
                "haskell" → exth "cabal update"
                _         → return () -- do nothing
      r <- case sc of
            [] → case lang of
                    "c"       → rxth "make"
                    "haskell" → rxth "cabal install"
                    "rust"    → rxth "make"
                    _         → return ExitSuccess
            _ → let scp []     = return ExitSuccess
                    scp [x]    = rxth x
                    scp (x:xs) = exth x >> scp xs
                in scp sc
      updateStatusIcon loc $ case r of
                              ExitSuccess → True
                              _           → False
   else when interactive
      $ let test fe procx previous = if previous
              then return True
              else doesFileExist (loc </> fe) ≫= \fileExist →
                      when fileExist procx
                      ≫ return fileExist
            cabal previous = if previous
              then return True
              else do getAll ← getDirectoryContents "."
                      let f = filter (\x → any (`isSuffixOf` map toLower x)
                                      [".cabal"]) getAll
                      if not (null f)
                        then do exth "cabal install --only-dependencies"
                                exth "cabal configure"
                                exth "cabal build"
                                return True
                        else return False
            ipkg previous = if previous
              then return True
              else do getAll ← getDirectoryContents "."
                      let f = filter (\x → any (`isSuffixOf` map toLower x)
                                      [".ipkg"]) getAll
                      if not (null f)
                        then do let f0 = head f
                                exth $ "idris --clean " ⧺ f0
                                exth $ "idris --install " ⧺ f0
                                return True
                        else return False
        in do s <- return False ≫= test "install.bat" (exth "install.bat")
                                ≫= test "build.bat" (exth "build.bat")
                                ≫= test "build.cmd" (exth "build.cmd")
                                ≫= test "Makefile" (exth "make")
                                ≫= cabal
                                ≫= ipkg
              updateStatusIcon loc s
  where exth cmd = setCurrentDirectory loc ≫ sys cmd
        rxth cmd = setCurrentDirectory loc ≫ system cmd
