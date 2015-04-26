{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Yaml
  ( FromJSON
  , ToJSON
  , yDecode
  , yEncode

  , module Model
  ) where

import Model
import Data.Yaml

import qualified Data.ByteString.Char8 as BS
import Control.Applicative.Unicode

instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .:  "location" ⊛
                           v .:  "task"     ⊛
                           v .:  "branches" ⊛
                           v .:  "upstream" ⊛
                           v .:? "enabled"  ⊛
                           v .:? "positive" ⊛
                           v .:? "clean"    ⊛
                           v .:? "postRebuild" ⊛
                           v .:? "group" ⊛
                           v .:? "hash"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Repository from YAML"

instance ToJSON Repository where
   toJSON (Repository loca tsk br up enb pos cln pr gr
                      hs) = object [ "location"     .= loca
                                   , "task"         .= tsk
                                   , "branches"     .= br
                                   , "upstream"     .= up
                                   , "enabled"      .= enb
                                   , "positive"     .= pos
                                   , "clean"        .= cln
                                   , "postRebuild"  .= pr
                                   , "group"        .= gr
                                   , "hash"         .= hs]

instance FromJSON Sharingan where
    parseJSON (Object v) = Sharingan <$>
                           v .:? "language" ⊛
                           v .:? "env" ⊛
                           v .:? "before_install" ⊛
                           v .:? "install" ⊛
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

instance FromJSON Defaults where
  parseJSON (Object v) = Defaults <$>
                         v .:? "quick"
  -- A non-Object value is of the wrong type, so fail.
  parseJSON _ = error "Can't parse Defaults from YAML"

instance ToJSON Defaults where
  toJSON (Defaults q) = object [ "quick" .= q ]

yDecode :: FromJSON iFromJSONable ⇒ FilePath → IO iFromJSONable
yDecode fnm = do
  ymlData ← BS.readFile fnm
  return $ case Data.Yaml.decode ymlData of
              Just decoded → decoded
              Nothing      → error "Can't parse from YAML"

yEncode :: ToJSON iToJSONable ⇒ FilePath → iToJSONable → IO()
yEncode fnm dat = BS.writeFile fnm $ Data.Yaml.encode dat
