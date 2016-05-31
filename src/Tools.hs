{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Tools
  ( cabalUpdate
  , stackUpdate
  ) where

import           System.Directory

import           Control.Eternal
import           Control.Exception
import           Control.Monad

import           Exec
import           FileSystem

cabalUpdate ∷ IO ()
cabalUpdate = (try $ exec "cabal update" :: IO (Either SomeException ()))
                ≫= \case Left _ → putStrLn "failed to cabal update"
                         Right _ → return()

stackUpdate ∷ IO ()
stackUpdate = (try $ exec "stack update" :: IO (Either SomeException ()))
                ≫= \case Left _ → putStrLn "failed to stack update"
                         Right _ → return()
