{-# LANGUAGE
    UnicodeSyntax
  , LambdaCase
  , Safe
  #-}

module Exec
  ( execute, exec, exc
  , sys, system
  , readCheck
  , readIfSucc
  ) where

import System.Directory (setCurrentDirectory)
import System.Process

import Trim
import Data.List.Split

import Control.Eternal.Syntax
import Control.Exception

-- |⊙)
-- |‿⊙)
-- |⊙‿⊙)
-- TODO: Handle all quotes, not just first command
-- (most common usecase is "C:\Program Files\git")
handleQuotes -- split command with quotes into command and arguments
  :: String  -- command with quotes...
   → (String, [String])
handleQuotes qq =
  if '\"' ∈ qq
    then let spq = filter (not ∘ null) $ splitOn "\"" qq
             nos = filter (not ∘ null ∘ trim) spq
             ssq = filter (not ∘ null) $ splitOn " " (concat (tail nos))
         in (head nos, ssq)
    else let spq = filter (not ∘ null) $ splitOn " " qq
             ssq = tail spq
         in (head spq, ssq)

-- Note don't use exec with & [Use execute]
execute       -- ✌(◉_◉ )
 :: [String] -- commands
  → IO()
execute [] = return ()
execute [x] =
  let (fs, sn) = handleQuotes x
  in rawSystem fs sn ≫ return ()
execute (x:xs) = do execute [x]
                    execute xs

exec       -- (҂￣‿‿￣҂)
 :: String -- command
  → IO()
exec cmd = execute commands
  where commands = splitOn "&" cmd

-- just run custm command as system
sys :: String → IO()
sys cmd = system cmd ≫ return ()

-- process exec in current directory
exc :: String → String → IO()
exc path args = setCurrentDirectory path ≫ exec args

readCheck    -- return whether command was success or not
 :: String   -- command
  → [String] -- arguments
  → IO (Either SomeException String)
readCheck cmd args = try $ readProcess cmd args []

readIfSucc   -- useless wrapper on readCheck to return Maybe
 :: String   -- command
  → [String] -- arguments
  → IO (Maybe String)
readIfSucc cmd args =
  readCheck cmd args
  ≫= \case Left _ → return Nothing
           Right val → do putStr $ cmd ⧺ " : " ⧺ val
                          return (Just val)
