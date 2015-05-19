{-# LANGUAGE UnicodeSyntax, LambdaCase #-}

module Exec
  ( exec
  , exc
  , sys
  , system
  , readIfSucc
  ) where

import System.Directory (setCurrentDirectory)
import System.Process (waitForProcess, rawSystem, system, readProcess)

import Trim
import Data.List.Split

import Control.Eternal.Syntax
import Control.Monad
import Control.Exception

-- |⊙)
-- |‿⊙)
-- |⊙‿⊙)
-- TODO: Handle all quotes, not just first command
handleQuotes :: String → (String, [String])
handleQuotes qq =
  if '\"' ∈ qq
    then let spq = filter (not ∘ null) $ splitOn "\"" qq
             nos = filter (not ∘ null ∘ trim) spq
             fsq = nos !! 0
             ssq = filter (not ∘ null) $ splitOn " " (concat (tail nos))
         in (fsq, ssq)
    else let spq = filter (not ∘ null) $ splitOn " " qq
             fsq = spq !! 0
             ssq = tail spq
         in (fsq, ssq)

-- ✌(◉_◉ )
execute :: [String] → IO()
execute [] = return ()
execute [x] =
  let (fs, sn) = handleQuotes x
  in rawSystem fs sn ≫ return ()
execute (x:xs) = do execute [x]
                    execute xs

-- (҂￣‿‿￣҂)
exec :: String → IO()
exec cmd = execute commands
  where commands = splitOn "&" cmd

sys :: String → IO()
sys cmd = system cmd ≫ return ()

exc :: String → String → IO()
exc path args = setCurrentDirectory path ≫ exec args

readCheck :: String → [String] → IO (Either SomeException String)
readCheck cmd args = try $ readProcess cmd args []

readIfSucc :: String → [String] → IO (Maybe String)
readIfSucc cmd args =
  readCheck cmd args
  ≫= \case Left _ → return Nothing
           Right val → do putStr $ cmd ⧺ " : " ⧺ val
                          return (Just cmd)
