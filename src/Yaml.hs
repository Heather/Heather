{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

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

import           Model

import qualified Data.ByteString.Char8       as BS
import           Data.Vector
import           Data.Yaml

import           Control.Applicative.Unicode

newtype RepositoryWrapper = RepositoryWrapper
  { _getRepository :: Repository }

instance FromJSON RepositoryWrapper where
    parseJSON (Object φ) = RepositoryWrapper <$>
      (Repository <$> φ .:  "location"
                    ⊛ φ .:  "task"
                    ⊛ φ .:  "branches"
                    ⊛ φ .:  "upstream"
                    ⊛ φ .:? "enabled"     .!= Just True
                    ⊛ φ .:? "root"        .!= Just False
                    ⊛ φ .:? "positive"    .!= Nothing
                    ⊛ φ .:? "clean"       .!= Nothing
                    ⊛ φ .:? "postRebuild" .!= Nothing
                    ⊛ φ .:? "group"       .!= Nothing
                    ⊛ φ .:? "hash"        .!= Nothing
                    ⊛ φ .:? "vcs"         .!= Nothing)
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
    parseJSON (Object φ) = SharinganWrapper <$>
      (Sharingan <$> φ .:? "language"       .!= Nothing
                   ⊛ φ .:? "env"            .!= Nothing
                   ⊛ φ .:? "before_install" .!= Nothing
                   ⊛ φ .:? "install"        .!= Nothing
                   ⊛ φ .: "script")
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
  parseJSON (Object φ) = DefaultsWrapper <$> (Defaults <$>
                         φ .:? "quick"         .!= Nothing
                         ⊛ φ .:? "full"        .!= Nothing
                         ⊛ φ .:? "cabalUpdate" .!= Nothing
                         ⊛ φ .:? "stackUpdate" .!= Nothing)
  parseJSON (Array array) = parseJSON (array ! 0)
  parseJSON _ = error "Can't parse Defaults from YAML"

instance ToJSON DefaultsWrapper where
  toJSON (DefaultsWrapper (Defaults ω ζ γ σ)) =
    object [ "quick" .= ω
           , "full"  .= ζ
           , "cabalUpdate" .= γ
           , "stackUpdate" .= σ
           ]

yDecode ∷ FromJSON λ ⇒ FilePath → IO λ
yDecode fName = do
  ymlData ← BS.readFile fName
  return $ case decodeEither ymlData of
                  Left ε  → error ε
                  Right ρ → ρ

yEncode ∷ ToJSON λ ⇒ FilePath → λ → IO()
yEncode fName δ = BS.writeFile fName $ encode δ
