{-# LANGUAGE OverloadedStrings #-}

module Yaml
  ( Repository(..)
  , Sharingan(..)
  , FromJSON
  , ToJSON
  , yDecode
  , yEncode
  ) where

import Data.Yaml
import Data.Maybe (fromJust)

import Control.Applicative

import qualified Data.ByteString.Char8 as BS

data Sharingan = Sharingan { language        :: Maybe String
                           , env             :: Maybe [String]
                           , before_install  :: Maybe [String]
                           , install         :: Maybe [String]
                           , script          :: [String]
                           } deriving (Show)

data Repository = Repository { location      :: String
                             , branches      :: [String]
                             , upstream      :: String
                             , enabled       :: Maybe Bool
                             , clean         :: Maybe Bool
                             , postRebuild  :: Maybe [String]
                             , syncGroup     :: Maybe String
                             , hash          :: Maybe String
                             } deriving (Show, Eq)

instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .:  "location" <*>
                           v .:  "branches" <*>
                           v .:  "upstream" <*>
                           v .:? "enabled"  <*>
                           v .:? "clean"    <*>
                           v .:? "postRebuild" <*>
                           v .:? "group" <*>
                           v .:? "hash"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Repository from YAML"

instance ToJSON Repository where
   toJSON (Repository loca br up enb cln pr gr
                      hs) = object [ "location"     .= loca
                                   , "branches"     .= br
                                   , "upstream"     .= up
                                   , "enabled"      .= enb
                                   , "clean"        .= cln
                                   , "postRebuild" .= pr
                                   , "group"        .= gr
                                   , "hash"         .= hs]

instance FromJSON Sharingan where
    parseJSON (Object v) = Sharingan <$>
                           v .:? "language" <*>
                           v .:? "env" <*>
                           v .:? "before_install" <*>
                           v .:? "install" <*>
                           v .: "script"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Sharingan from YAML"

instance ToJSON Sharingan where
   toJSON (Sharingan lan en before inst
                     sc) = object [ "language"        .= lan
                                  , "env"             .= en
                                  , "before_install"  .= before
                                  , "install"         .= inst
                                  , "script"          .= sc]

yDecode :: FromJSON iFromJSONable => FilePath -> IO iFromJSONable
yDecode fnm = do
    ymlData <- BS.readFile fnm
    return $ case Data.Yaml.decode ymlData of
                Just decoded -> decoded
                Nothing      -> error "Can't parse from YAML"

yEncode :: ToJSON iToJSONable => FilePath -> iToJSONable -> IO()
yEncode fnm dat = BS.writeFile fnm $ Data.Yaml.encode dat
