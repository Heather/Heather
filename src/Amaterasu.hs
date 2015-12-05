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

amaterasu :: String → String → String → [String]
           → Bool → Bool → Bool → Bool → Maybe String
           → Bool → MyEnv → IO (Bool, Bool)
amaterasu "rebase"  = rebasefork
amaterasu "pull"    = pull
amaterasu custom    = \path _ _ _ _ _ adm _ _ _ →
  doesDirectoryExist path ≫= \dirExist →
    if dirExist then setCurrentDirectory path ≫ do
                        exec $ ifadmin adm ⧺ custom
                        return (True, True)
                else return (False, False)
