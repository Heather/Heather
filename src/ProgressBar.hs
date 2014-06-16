module ProgressBar
  ( putProgress
  , drawProgressBar
  , drawPercentage
  ) where

import Control.Monad
import Control.Concurrent
import System.IO
import Text.Printf

putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ "\r\ESC" ++ s

drawProgressBar :: Int -> Rational -> String
drawProgressBar width progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral width)
        spaces = width - bars

drawPercentage :: Rational -> String
drawPercentage progress = printf "%3d%%" (truncate (progress * 100) :: Int)