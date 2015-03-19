{-# LANGUAGE UnicodeSyntax #-}
module Exec
  ( exec
  , exc
  ) where

import System.Directory (setCurrentDirectory)
import System.Process (waitForProcess, rawSystem)

import Data.List.Split

import Control.Eternal.Syntax

--TODO: quotes support
execute :: [String] → IO()
execute [] = return ()
execute [x] = 
    let qqq = filter (/= '\"') x -- temporary just filter it out
        spl = filter (not . null) $ splitOn " " qqq
        fs = spl !! 0
        sn = tail spl
    in rawSystem fs sn ≫ return ()
execute (x:xs) = do execute [x]
                    execute xs

exec :: String → IO()
exec args = 
    let commands = splitOn "&" args
    in execute commands

exc :: String → String → IO()
exc path args = setCurrentDirectory path ≫ exec args
