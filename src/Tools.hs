{-# LANGUAGE
    UnicodeSyntax
  , LambdaCase
  #-}

module Tools
  ( cabalUpdate
  , stackUpdate
  ) where

import System.Directory

import Control.Monad
import Control.Exception
import Control.Eternal

import FileSystem
import Exec

cabalUpdate :: IO ()
cabalUpdate = (try $ exec "cabal update" :: IO (Either SomeException ()))
                ≫= \case Left _ → putStrLn "failed to cabal update"
                         Right _ → return()

stackUpdate :: IO ()
stackUpdate = (try $ exec "stack update" :: IO (Either SomeException ()))
                ≫= \case Left _ → putStrLn "failed to stack update"
                         Right _ → return()
