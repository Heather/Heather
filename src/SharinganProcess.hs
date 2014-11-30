{-# LANGUAGE CPP #-}

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

sharingan :: Bool -> String -> String -> Bool -> IO()
sharingan interactive shx loc shxi = if shxi then
     do syncDatax <- yDecode shx :: IO Sharingan
        let en   = fromMaybe [] (env syncDatax)
            be   = fromMaybe [] (before_install syncDatax)
            sc   = script syncDatax
            lang = case language syncDatax of
                        Just [] -> []
                        Just ln -> map toLower ln
                        _ -> []
        forM_ en $ setEnv
        forM_ be $ exc loc
        case install syncDatax of
          Just []  -> return () -- do nothing
          Just ilX -> forM_ ilX $ exc loc
          _ -> case lang of
                  "haskell" -> exc loc "cabal update"
                  _         -> return () -- do nothing
        case sc of
          [] -> case lang of
                  "c"       -> exc loc "make"
                  "haskell" -> exc loc "cabal install"
                  "rust"    -> exc loc "make"
                  _         -> return () -- do nothing
          _ -> forM_ sc $ exc loc
     else when interactive
        $ let test fe procx previous = if previous
                then return True
                else doesFileExist (loc </> fe) >>= \fileExist ->
                        when fileExist procx
                        >> return fileExist
              cabal previous = if previous
                then return True
                else do all <- getDirectoryContents "."
                        let f = filter (\x -> any(`isSuffixOf` map toLower x)
                                        [".cabal"]) $ all
                        if (length f) > 0
                          then do exc loc "cabal install --only-dependencies"
                                  exc loc "cabal configure"
                                  exc loc "cabal build"
                                  return True
                          else return False 
              ipkg previous = if previous
                then return True
                else do all <- getDirectoryContents "."
                        let f = filter (\x -> any(`isSuffixOf` map toLower x)
                                        [".ipkg"]) $ all
                        if (length f) > 0
                          then do let f0 = f !! 0
                                  exc loc $ "idris --clean " ++ f0
                                  exc loc $ "idris --install " ++ f0
                                  return True
                          else return False
          in (return False) >>= test "install.bat" (exc loc "install.bat")
                            >>= test "build.bat" (exc loc "build.bat")
                            >>= test "Makefile" (exc loc "make")
                            >>= cabal
                            >>= ipkg
                            >> return ()
