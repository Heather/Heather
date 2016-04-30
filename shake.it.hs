{-# LANGUAGE UnicodeSyntax #-}

import Shake.It.Off
import System.Process
import Control.Monad

main :: IO ()
main = shake $ do
  "clean" ∫ cabal ["clean"]

  sharinganExecutable ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  "force" ∫ do
    cabal ["install", "--force-reinstalls", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  "install" ∫ cabal ["install"]

  "force-reinstall" ∫
    cabal ["--force-reinstalls", "install"]

  "test" ◉ [sharinganExecutable] ∰ do
    rawSystem sharinganExecutable ["--version"] >>= checkExitCode
    return ()

 where buildPath :: String
       buildPath = "dist/build/Sharingan"

       sharinganExecutable :: String
       sharinganExecutable = buildPath </> "sharingan.exe"
