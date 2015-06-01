{-# LANGUAGE UnicodeSyntax #-}

module HTTP
  ( getHTTP
  , download
  ) where

import Network.Socket (withSocketsDo)
import Network.HTTP.Conduit

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Types

import qualified Data.Conduit as C
import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S

import Control.Monad.IO.Class (liftIO)

getHTTP :: [Char] → IO String
getHTTP url = withSocketsDo
  $ simpleHttp url
      >>= \bs → return $ S.decode $ L.unpack bs

download :: String → String → IO()
download url filename = withSocketsDo $ do
  irequest ← liftIO $ parseUrl url
  withManager $ \manager → do
    let request = irequest
         { method = methodGet
         , responseTimeout = Just 10000000 }
    response ← http request manager
    responseBody response C.$$+- sinkFile filename
