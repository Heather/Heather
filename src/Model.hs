{-# LANGUAGE Safe          #-}
{-# LANGUAGE UnicodeSyntax #-}

module Model
    ( Repository(..)
    , Sharingan(..)
    , Defaults(..)
    , Args(..)
    , CommonOpts(..)
    , AddOpts(..)
    , SyncOpts(..)
    , Command(..)
    , MyEnv(..)

    , module MX
    ) where

import           Control.Applicative as MX
import           Control.Eternal     as MX
import           Control.Monad       as MX

data Sharingan = Sharingan
    { language      ∷ Maybe String
    , env           ∷ Maybe [String]
    , beforeInstall ∷ Maybe [String]
    , install       ∷ Maybe [String]
    , script        ∷ [String]
    } deriving (Show)

data Repository = Repository
    { location    ∷ String
    , task        ∷ String
    , branches    ∷ [String]
    , upstream    ∷ String
    , enabled     ∷ Maybe Bool
    , root        ∷ Maybe Bool
    , positive    ∷ Maybe Bool
    , clean       ∷ Maybe Bool
    , postRebuild ∷ Maybe [String]
    , syncGroup   ∷ Maybe String
    , hash        ∷ Maybe String
    , vcs         ∷ Maybe String
    } deriving (Show, Eq)

data Defaults = Defaults
    { quick       ∷ Maybe Bool
    , full        ∷ Maybe Bool
    , updateCabal ∷ Maybe Bool
    , updateStack ∷ Maybe Bool
    }

data Args = Args CommonOpts Command
      deriving Show

data CommonOpts = CommonOpts
    { optVerbosity ∷ Bool
    , optJobs      ∷ Int
    } deriving Show

data AddOpts = AddOpts
    { sType   ∷ Maybe String
    , sFilter ∷ Maybe String
    , sGroup  ∷ Maybe String
    } deriving Show

data SyncOpts = SyncOpts
    { syncFull        ∷ Bool
    , syncForce       ∷ Bool
    , syncUnsafe      ∷ Bool
    , syncQuick       ∷ Bool
    , syncInteractive ∷ Bool
    , syncFilter      ∷ Maybe String
    , syncGroups      ∷ [String]
    , syncNoPush      ∷ Bool
    } deriving Show

data Command
    = Sync SyncOpts
    | MakeSharingan
    | Config
    | DefaultsConf
    | List [String]
    | Status [String]
    | Add AddOpts | Delete [String]
    | Enable String | Disable String
    | Depot
    | Gentoo
    deriving Show

data MyEnv = MyEnv
    { git ∷ String
    , hg  ∷ String
    } deriving Show
