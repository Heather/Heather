module Model
  ( Repository(..)
  , Sharingan(..)
  , Defaults(..)  
  ) where

data Sharingan = Sharingan { language        :: Maybe String
                           , env             :: Maybe [String]
                           , before_install  :: Maybe [String]
                           , install         :: Maybe [String]
                           , script          :: [String]
                           } deriving (Show)

data Repository = Repository { location      :: String
                             , task          :: String
                             , branches      :: [String]
                             , upstream      :: String
                             , enabled       :: Maybe Bool
                             , clean         :: Maybe Bool
                             , postRebuild   :: Maybe [String]
                             , syncGroup     :: Maybe String
                             , hash          :: Maybe String
                             } deriving (Show, Eq)

data Defaults = Defaults { quick :: Maybe Bool
                         }
