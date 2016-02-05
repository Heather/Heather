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

  "install" ◉ [sharinganExecutable] ∰
    cabal ["install"]

  "test" ◉ [sharinganExecutable] ∰ do
    rawSystem sharinganExecutable ["--version"] >>= checkExitCode
    return ()

 where buildPath :: String
       buildPath = "dist/build/Sharingan"

       sharinganExecutable :: String
       sharinganExecutable = buildPath </> "sharingan.exe"
