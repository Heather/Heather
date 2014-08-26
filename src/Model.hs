module Model
  ( Options(..)
  ) where

data Options = Options 
    { optJobs :: Maybe String,      optSync :: Maybe String
    , optSyncGroup :: Maybe String, optInteractive :: Bool
    , optG :: Bool,                 optForce :: Bool
    , optUnsafe :: Bool
    , optFast :: [String] -> Bool -> Bool -> Bool -> Maybe String -> Maybe String -> Maybe String -> IO()
    }
