module Depot
  ( getDepotTools
  ) where

import Network.Socket
import Network.HTTP.Conduit

import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Types

import qualified Data.Conduit as C
import Control.Monad.IO.Class (liftIO)
{-------------------------------  Depot Tools  -----------------------------------------}
getDepotTools :: IO()
getDepotTools = withSocketsDo $ do
    let url = "http://src.chromium.org/svn/trunk/tools/depot_tools.zip"
    irequest <- liftIO $ parseUrl url
    withManager $ \manager -> do
        let request = irequest
             { method = methodGet }
        response <- http request manager
        responseBody response C.$$+- sinkFile "depot_tools.zip"
{----------------------------------------------------------------------------------------}
