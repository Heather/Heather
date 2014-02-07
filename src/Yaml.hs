{-# LANGUAGE OverloadedStrings #-}

module Yaml
  ( Repository,
    Sharingan,
    script,
    location,
    branches,
    upstream,
    FromJSON,
    yDecode
  ) where

import Data.Yaml
import Data.Maybe (fromJust)

import Control.Applicative

import qualified Data.ByteString.Char8 as BS

data Sharingan = Sharingan {script :: [String]}
                            deriving (Show)

data Repository = Repository {location :: String,
                              branches :: [String],
                              upstream :: String}
                              deriving (Show)

instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .: "location" <*>
                           v .: "branches" <*>
                           v .: "upstream"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Repository from YAML/JSON"

instance FromJSON Sharingan where
    parseJSON (Object v) = Sharingan <$>
                           v .: "script"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Sharingan from YAML/JSON"

yDecode :: FromJSON iFromJSONable => FilePath -> IO iFromJSONable
yDecode fnm = do
    ymlData <- BS.readFile fnm
    return $ fromJust $ Data.Yaml.decode ymlData
