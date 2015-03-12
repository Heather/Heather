{-# LANGUAGE CPP, UnicodeSyntax #-}

module SharinganProcess
  ( sharingan
  , amaterasu
#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  , gentooSync
#endif
  ) where

import Yaml
import Shell

import System.Directory
import System.FilePath(takeDirectory, (</>))

import Data.Char

sharingan :: Bool → String → String → Bool → IO()
sharingan interactive shx loc shxi = if shxi then     
     do syncDatax ← yDecode shx :: IO Sharingan
        let sc   = script syncDatax
            lang = case language syncDatax of
                        Just [] → []
                        Just ln → map toLower ln
                        _ → []
        forM_ (fromMaybe [] (env syncDatax)) setEnv
        forM_ (fromMaybe [] (before_install syncDatax)) exth
        case install syncDatax of
          Just []  → return () -- do nothing
          Just ilX → forM_ ilX exth
          _ → case lang of
                  "haskell" → exth "cabal update"
                  _         → return () -- do nothing
        case sc of
          [] → case lang of
                  "c"       → exth "make"
                  "haskell" → exth "cabal install"
                  "rust"    → exth "make"
                  _         → return () -- do nothing
          _ → forM_ sc exth
     else when interactive
        $ let test fe procx previous = if previous
                then return True
                else doesFileExist (loc </> fe) ≫= \fileExist →
                        when fileExist procx
                        ≫ return fileExist
              cabal previous = if previous
                then return True
                else do all ← getDirectoryContents "."
                        let f = filter (\x → any (`isSuffixOf` map toLower x)
                                        [".cabal"]) $ all
                        if (length f) > 0
                          then do exth "cabal install --only-dependencies"
                                  exth "cabal configure"
                                  exth "cabal build"
                                  return True
                          else return False 
              ipkg previous = if previous
                then return True
                else do all ← getDirectoryContents "."
                        let f = filter (\x → any (`isSuffixOf` map toLower x)
                                        [".ipkg"]) $ all
                        if (length f) > 0
                          then do let f0 = f !! 0
                                  exth $ "idris --clean " ⧺ f0
                                  exth $ "idris --install " ⧺ f0
                                  return True
                          else return False
          in (return False) ≫= test "install.bat" (exth "install.bat")
                            ≫= test "build.bat" (exth "build.bat")
                            ≫= test "build.cmd" (exth "build.cmd")
                            ≫= test "Makefile" (exth "make")
                            ≫= cabal
                            ≫= ipkg
                            ≫ return ()
  where exth = exc loc
