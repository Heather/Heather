{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiWayIf    #-}

import           Control.Monad
import           Shake.It.Off
import           System.Process

main ∷ IO ()
main = shake $ do
  "clean" ∫ cabal ["clean"]

  sharinganExecutable ♯ do
    cabal ["install", "--only-dependencies"]
    cabal ["configure"]
    cabal ["build"]

  "stack" ∫ stack ["build"]

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

 where buildPath ∷ String
       buildPath = "dist/build/sharingan"

       sharinganExecutable ∷ String
       sharinganExecutable =
         if | os ∈ ["win32", "mingw32", "cygwin32"] → buildPath </> "sharingan.exe"
            | otherwise → buildPath </> "sharingan"
