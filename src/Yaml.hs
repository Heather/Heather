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
import qualified Data.ByteString.Char8 as BS

import Control.Applicative.Unicode

newtype RepositoryWrapper = RepositoryWrapper
  { _getRepository :: Repository }

instance FromJSON RepositoryWrapper where
    parseJSON (Object v) = RepositoryWrapper <$>
      (Repository <$> v .:  "location"
                    ⊛ v .:  "task"
                    ⊛ v .:  "branches"
                    ⊛ v .:  "upstream"
                    ⊛ v .:? "enabled"     .!= Just True
                    ⊛ v .:? "root"        .!= Just False
                    ⊛ v .:? "positive"    .!= Nothing
                    ⊛ v .:? "clean"       .!= Nothing
                    ⊛ v .:? "postRebuild" .!= Nothing
                    ⊛ v .:? "group"       .!= Nothing
                    ⊛ v .:? "hash"        .!= Nothing
                    ⊛ v .:? "vcs"         .!= Nothing)
    parseJSON (Array array) = parseJSON (array ! 0)
    parseJSON _ = error "Can't parse Repository from YAML"

instance ToJSON RepositoryWrapper where
   toJSON (RepositoryWrapper (Repository loca tsk br up enb
                                         root pos cln pr gr hs
                      vc)) = object [ "location"     .= loca
                                    , "task"         .= tsk
                                    , "branches"     .= br
                                    , "upstream"     .= up
                                    , "enabled"      .= enb
                                    , "root"         .= root
                                    , "positive"     .= pos
                                    , "clean"        .= cln
                                    , "postRebuild"  .= pr
                                    , "group"        .= gr
                                    , "hash"         .= hs
                                    , "vcs"          .= vc]

newtype SharinganWrapper = SharinganWrapper
  { _getSharingan :: Sharingan }

instance FromJSON SharinganWrapper where
    parseJSON (Object v) = SharinganWrapper <$>
      (Sharingan <$> v .:? "language"       .!= Nothing
                   ⊛ v .:? "env"            .!= Nothing
                   ⊛ v .:? "before_install" .!= Nothing
                   ⊛ v .:? "install"        .!= Nothing
                   ⊛ v .: "script")
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
                         v .:? "quick"         .!= Nothing
                         ⊛ v .:? "full"        .!= Nothing
                         ⊛ v .:? "cabalUpdate" .!= Nothing
                         ⊛ v .:? "stackUpdate" .!= Nothing)
  parseJSON (Array array) = parseJSON (array ! 0)
  parseJSON _ = error "Can't parse Defaults from YAML"

instance ToJSON DefaultsWrapper where
  toJSON (DefaultsWrapper (Defaults q f c s)) =
    object [ "quick" .= q
           , "full"  .= f
           , "cabalUpdate" .= c
           , "stackUpdate" .= s
           ]

yDecode :: FromJSON iFromJSONable ⇒ FilePath → IO iFromJSONable
yDecode fnm = do
  ymlData ← BS.readFile fnm
  return $ case decodeEither ymlData of
                  Left er → error er
                  Right r → r

yEncode :: ToJSON iToJSONable ⇒ FilePath → iToJSONable → IO()
yEncode fnm dat = BS.writeFile fnm $ encode dat
