{-# LANGUAGE CPP           #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Amaterasu
  ( amaterasu
  , setEnv
  , module Exec
  ) where

import           Data.List
import           Data.List.Split
import           Data.Maybe

import           System.Directory
import           System.FilePath  ((</>))
import           System.Info      (os)

import           System.Process

import           Config
import           Exec
import           Trim

import           Shell.Helper
import           Shell.Pull
import           Shell.Rebase

amaterasu
  ∷ String           --repo task
   → String           -- location
   → String           -- branch
   → [String]         -- splitted upstream (splitOn " " $ upstream repo)
   → Bool             -- unsafe
   → Bool             -- force
   → Bool             -- clean
   → Bool             -- admin (sudo)
   → Maybe String     -- Hash
   → MyEnv            -- environment
   → Maybe String     -- VCS
   → IO (Bool, Bool)  -- success & continue
amaterasu "rebase"  = rebasefork
amaterasu "pull"    = pull
amaterasu custom    = \path _ _ _ _ _ adm _ _ _ →
  doesDirectoryExist path ≫= \dirExist →
    if dirExist then setCurrentDirectory path ≫ do
                        prefix ← ifadmin adm
                        exec $ prefix ⧺ custom
                        return (True, True)
                else return (False, False)
