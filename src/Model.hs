{-# LANGUAGE UnicodeSyntax #-}

module Model
    ( Repository(..)
    , Sharingan(..)
    , Defaults(..)  
    , Args(..)
    , CommonOpts(..)
    , SyncOpts(..)
    , Command(..)
    , MyEnv(..)
  
    , module Control.Monad
    , module Control.Applicative
    , module Control.Exception
    , module Control.Eternal
  
    , module Data.Maybe
    , module Data.List
    ) where

import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Eternal

import Data.Maybe
import Data.List

data Sharingan = Sharingan
    { language        ∷ Maybe String
    , env             ∷ Maybe [String]
    , before_install  ∷ Maybe [String]
    , install         ∷ Maybe [String]
    , script          ∷ [String]
    } deriving (Show)

data Repository = Repository
    { location      ∷ String
    , task          ∷ String
    , branches      ∷ [String]
    , upstream      ∷ String
    , enabled       ∷ Maybe Bool
    , positive      ∷ Maybe Bool
    , clean         ∷ Maybe Bool
    , postRebuild   ∷ Maybe [String]
    , syncGroup     ∷ Maybe String
    , hash          ∷ Maybe String
    } deriving (Show, Eq)

data Defaults = Defaults
    { quick ∷ Maybe Bool
    }

data Args = Args CommonOpts Command
      deriving Show
  
data CommonOpts = CommonOpts
    { optVerbosity ∷ Bool
    , optJobs ∷ Int
    } deriving Show
    
data SyncOpts = SyncOpts
    { syncFull ∷ Bool
    , syncForce ∷ Bool
    , syncUnsafe ∷ Bool
    , syncQuick ∷ Bool
    , syncInteractive ∷ Bool
    , syncFilter ∷ Maybe String
    , syncGroups ∷ [String]
    } deriving Show

data Command
    = Sync SyncOpts
    | MakeSharingan
    | Config
    | DefaultsConf
    | List [String]
    | Status [String]
    | Add [String] | Delete [String]
    | Enable String | Disable String
    | Depot | Cabal
    | Gentoo
    deriving Show

data MyEnv = MyEnv
    { git ∷ String
    , hg ∷ String
    } deriving Show
