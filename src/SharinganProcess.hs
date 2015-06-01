{-# LANGUAGE CPP, MultiWayIf, UnicodeSyntax #-}

module SharinganProcess
  ( sharingan
  , amaterasu
#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
  , gentooSync
#endif
  ) where

import Yaml
import Config
import Shell

import System.Directory
import System.FilePath ((</>))
import System.Exit

import Data.Char

updateStatusIcon ∷ String → Bool → IO()
updateStatusIcon loc pos =
  withConfig $ \ymlx →
    let ymlprocess = ifSo $ do
        rsdata ← yDecode ymlx ∷ IO [Repository]
        let fr x = if isInfixOf loc $ location x
                    then x { positive = Just pos }
                    else x
        yEncode ymlx $ map fr rsdata
    in doesFileExist ymlx ≫= ymlprocess

sharingan ∷ Bool → String → String → Bool → IO()
sharingan interactive shx loc shxi = if shxi then
   do syncDatax ← yDecode shx ∷ IO Sharingan
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
      r <- case sc of
            [] → case lang of
                    "c"       → rxth "make"
                    "haskell" → rxth "cabal install"
                    "rust"    → rxth "make"
                    _         → return ExitSuccess
            _ → let scp []     = return ExitSuccess
                    scp (x:[]) = rxth x
                    scp (x:xs) = do exth x
                                    scp xs
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
                                      [".cabal"]) $ getAll
                      if (length f) > 0
                        then do exth "cabal install --only-dependencies"
                                exth "cabal configure"
                                exth "cabal build"
                                return True
                        else return False
            ipkg previous = if previous
              then return True
              else do getAll ← getDirectoryContents "."
                      let f = filter (\x → any (`isSuffixOf` map toLower x)
                                      [".ipkg"]) $ getAll
                      if (length f) > 0
                        then do let f0 = f !! 0
                                exth $ "idris --clean " ⧺ f0
                                exth $ "idris --install " ⧺ f0
                                return True
                        else return False
        in do s <- (return False) ≫= test "install.bat" (exth "install.bat")
                                  ≫= test "build.bat" (exth "build.bat")
                                  ≫= test "build.cmd" (exth "build.cmd")
                                  ≫= test "Makefile" (exth "make")
                                  ≫= cabal
                                  ≫= ipkg
              updateStatusIcon loc s
  where exth cmd = setCurrentDirectory loc ≫ sys cmd
        rxth cmd = setCurrentDirectory loc ≫ system cmd
