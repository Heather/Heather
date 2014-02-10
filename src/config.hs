{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Repository,
    location,
    branch,
    upstream,
    FromJSON
  ) where

import Data.Yaml
import Data.Maybe (fromJust)

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Exception

{----------------------------------------------------------------------------------------}
data Repository = Repository {location :: String,
                              branch :: String,
                              upstream :: String}
                              deriving (Show)
{----------------------------------------------------------------------------------------}
instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .: "location" <*>
                           v .: "branch" <*>
                           v .: "upstream"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Repository from YAML/JSON"
{----------------------------------------------------------------------------------------}