{-# LANGUAGE
    OverloadedStrings
  , UnicodeSyntax
  #-}

module Yaml
  ( FromJSON
  , ToJSON

  , RepositoryWrapper(..)
  , SharinganWrapper(..)
  , DefaultsWrapper(..)

  , yDecode
  , yEncode

  , module Model
  ) where

import Model

import Data.Yaml
import Data.Vector
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative.Unicode

newtype RepositoryWrapper = RepositoryWrapper
  { _getRepository :: Repository }

instance FromJSON RepositoryWrapper where
    parseJSON (Object v) = RepositoryWrapper <$> (Repository <$>
                           v .:  "location"     ⊛
                           v .:  "task"         ⊛
                           v .:  "branches"     ⊛
                           v .:  "upstream"     ⊛
                           v .:? "enabled"      ⊛
                           v .:? "root"         ⊛
                           v .:? "positive"     ⊛
                           v .:? "clean"        ⊛
                           v .:? "postRebuild"  ⊛
                           v .:? "group"        ⊛
                           v .:? "hash"         )
    parseJSON (Array array) = parseJSON (array ! 0)
    parseJSON _ = error "Can't parse Repository from YAML"

instance ToJSON RepositoryWrapper where
   toJSON (RepositoryWrapper (Repository loca tsk br up enb root pos cln pr gr
                      hs)) = object [ "location"     .= loca
                                    , "task"         .= tsk
                                    , "branches"     .= br
                                    , "upstream"     .= up
                                    , "enabled"      .= enb
                                    , "root"         .= root
                                    , "positive"     .= pos
                                    , "clean"        .= cln
                                    , "postRebuild"  .= pr
                                    , "group"        .= gr
                                    , "hash"         .= hs]

newtype SharinganWrapper = SharinganWrapper
  { _getSharingan :: Sharingan }

instance FromJSON SharinganWrapper where
    parseJSON (Object v) = SharinganWrapper <$> (Sharingan <$>
                           v .:? "language"       ⊛
                           v .:? "env"            ⊛
                           v .:? "before_install" ⊛
                           v .:? "install"        ⊛
                           v .: "script"          )
    parseJSON (Array array) = parseJSON (array ! 0)
    parseJSON _ = error "Can't parse Sharingan from YAML"

instance ToJSON SharinganWrapper where
   toJSON (SharinganWrapper (Sharingan lan en before inst
                     sc)) = object [ "language"        .= lan
                                   , "env"             .= en
                                   , "before_install"  .= before
                                   , "install"         .= inst
                                   , "script"          .= sc]

newtype DefaultsWrapper = DefaultsWrapper
  { _getDefaults :: Defaults }

instance FromJSON DefaultsWrapper where
  parseJSON (Object v) = DefaultsWrapper <$> (Defaults <$>
                         v .:? "quick")
  parseJSON (Array array) = parseJSON (array ! 0)
  parseJSON _ = error "Can't parse Defaults from YAML"

instance ToJSON DefaultsWrapper where
  toJSON (DefaultsWrapper (Defaults q)) = object [ "quick" .= q ]

yDecode :: FromJSON iFromJSONable ⇒ FilePath → IO iFromJSONable
yDecode fnm = do
  ymlData ← BS.readFile fnm
  return $ fromMaybe <| error "Can't parse from YAML"
                     <| decode ymlData

yEncode :: ToJSON iToJSONable ⇒ FilePath → iToJSONable → IO()
yEncode fnm dat = BS.writeFile fnm $ encode dat
