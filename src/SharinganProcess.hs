module SharinganProcess
  ( sharingan,
    gentooSync,
    rebasefork
  ) where

import Yaml
import Shell

import System.Directory
import System.FilePath(takeDirectory, (</>))

import Data.Char
import Data.List

import Control.Monad
import Control.Eternal

sharingan :: Bool -> String -> String -> Bool -> IO()
sharingan intera shx loc shxi = if shxi then
     do syncDatax <- yDecode shx :: IO Sharingan
        let lang = map toLower $ language syncDatax
            en = env syncDatax
            be = before_install syncDatax
            il = install syncDatax
            sc = script syncDatax
        forM_ en $ setEnv
        forM_ be $ exc loc
        case il of
          [] -> case lang of
                  "haskell" -> exc loc "cabal update"
                  _         -> return () -- do nothing
          _ -> forM_ il $ exc loc
        case sc of
          [] -> case lang of
                  "c"       -> exc loc "make"
                  "haskell" -> exc loc "cabal install"
                  "rust"    -> exc loc "make"
                  _         -> return () -- do nothing
          _ -> forM_ sc $ exc loc
     else when intera
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
                          then do let build = do
                                      exc loc "cabal install --only-dependencies"
                                      exc loc "cabal configure"
                                      exc loc "cabal build"
                                  build
                                  return True
                          else return False
              make = exc loc "make"
              bat  = exc loc "build.bat"
          in (return False) >>= test "build.bat" bat
                            >>= test "Makefile" make
                            >>= cabal
                            >> return ()
