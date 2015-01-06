{-# LANGUAGE
    MultiWayIf
  , LambdaCase
  , CPP
  , UnicodeSyntax
  #-}

module Amaterasu
  ( amaterasu
  , setEnv
  , module Exec
  ) where

import Data.List.Split
import Data.Maybe
import Data.List

import System.Info (os)
import System.Directory
import System.FilePath((</>))

import System.Process

import Trim
import Exec
import Config

import Shell.Helper
import Shell.Pull
import Shell.Rebase

amaterasu
  :: String           --repo task
   → String           -- location
   → String           -- branch
   → [String]         -- splitted upstream (splitOn " " $ upstream repo)
   → Bool             -- unsafe
   → Bool             -- force
   → Bool             -- clean
   → Bool             -- admin (sudo)
   → Maybe String     -- Hash
   → MyEnv            -- environment
   → IO (Bool, Bool)  -- success & continue
amaterasu "rebase"  = rebasefork
amaterasu "pull"    = pull
amaterasu custom    = \path _ _ _ _ _ adm _ _ →
  doesDirectoryExist path ≫= \dirExist →
    if dirExist then setCurrentDirectory path ≫ do
                        exec $ ifadmin adm ⧺ custom
                        return (True, True)
                else return (False, False)
