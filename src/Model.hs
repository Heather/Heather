module Model
  ( Repository(..)
  , Sharingan(..)
  , Defaults(..)  
  , Args(..)
  , CommonOpts(..)
  , SyncOpts(..)
  , Command(..)
  ) where

data Sharingan = Sharingan 
    { language        :: Maybe String
    , env             :: Maybe [String]
    , before_install  :: Maybe [String]
    , install         :: Maybe [String]
    , script          :: [String]
    } deriving (Show)

data Repository = Repository 
    { location      :: String
    , task          :: String
    , branches      :: [String]
    , upstream      :: String
    , enabled       :: Maybe Bool
    , clean         :: Maybe Bool
    , postRebuild   :: Maybe [String]
    , syncGroup     :: Maybe String
    , hash          :: Maybe String
    } deriving (Show, Eq)

data Defaults = Defaults 
    { quick :: Maybe Bool
    }

data Args = Args CommonOpts Command
      deriving Show
  
data CommonOpts = CommonOpts
    { optVerbosity :: Bool
    , optJobs :: Int
    } deriving Show
    
data SyncOpts = SyncOpts
    { syncForce :: Bool
    , syncUnsafe :: Bool
    , syncQuick :: Bool
    , syncInteractive :: Bool
    , syncFilter :: [String]
    , syncGroups :: [String]
    } deriving Show

data Command
    = Sync SyncOpts
    | MakeSharingan
    | Config
    | DefaultsConf
    | List [String]
    | Add [String] | Delete [String]
    | Enable String | Disable String
    | Depot
    | Gentoo
    deriving Show
