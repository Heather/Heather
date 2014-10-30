{-# LANGUAGE MultiWayIf, LambdaCase #-}

module Shell
  ( exec,
    exc,
    setEnv,
    gpull,
    gclone,
    rebasefork,
    gentooSync
  ) where

import Data.List
import Data.Maybe
import Data.List.Split

import System.IO
import System.Info (os)
import System.Directory
import System.FilePath((</>))

import System.Process

import Control.Monad
import Control.Eternal

import Config
import AsyncReactive

rebasefork :: String -> String -> [String] -> Bool -> Bool -> Maybe String -> Bool -> IO (Bool, Bool)
rebasefork path branch up unsafe processClean rhash sync =
    let upstream = intercalate " " up
    in doesDirectoryExist path >>= \dirExist ->
        let chk foo (previous,doU) = if previous
                then return (previous, doU)
                else foo

            gitX = doesDirectoryExist <| path </> ".git" >>= \git ->
                    if git then if dirExist
                            then setCurrentDirectory path >> do
                                currentbranch <- readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] []
                                let cbr = trim currentbranch
                                    whe c s = when c $ exec s
                                whe (cbr /= branch) $ "git checkout " ++ branch
                                whe (not unsafe)    $ "git reset --hard & git rebase --abort"
                                whe (processClean)  $ "git clean -xdf"
                                loc <- case rhash of
                                        Just hsh -> return hsh
                                        _ -> if (length up) > 1
                                                then if sync then readProcess "git" [ "merge-base"
                                                                                    , up !! 1
                                                                                    , "origin/" ++ branch
                                                                                    ] []
                                                             else readProcess "git" [ "rev-parse"
                                                                                    , intercalate "/" up
                                                                                    ] []
                                                else readProcess "git" ["log", "-n", "1"
                                                                       , "--pretty=format:%H"
                                                                       ] []
                                rem <- readProcess "git" (["ls-remote"] ++ up) []
                                let remote = (splitOn "\t" rem) !! 0
                                    local  = trim loc
                                putStrLn $ "Last Merge: "  ++ local
                                putStrLn $ "Remote: " ++ remote
                                if remote == local
                                    then do putStrLn $ path ++ " is up to date"
                                            return (True, False)
                                    else do rlc <- readProcess "git" ["ls-remote", "origin", branch] []
                                            lrc <- if isNothing rhash then readProcess "git" ["log", "-n", "1"
                                                                                             , "--pretty=format:%H"
                                                                                             ] []
                                                                      else return loc
                                            let remloc = (splitOn "\t" rlc) !! 0
                                                locloc = trim lrc
                                            putStrLn $ "Origin: " ++ remloc
                                            putStrLn $ "Local: "  ++ locloc
                                            whe (remloc /= locloc) $ "git pull origin " ++ branch
                                            exec $ " git fetch "                  ++ upstream
                                                 ++ " & git pull --rebase "       ++ upstream
                                                 ++ " & git push --force origin " ++ branch
                                            hashupdate remote path
                                            return (True, True)
                            else return (True, True)
                           else  return (False, False)

            hgX = doesDirectoryExist <| path </> ".hg" >>= \hg ->
                    if hg then if dirExist
                            then setCurrentDirectory path >> do
                                exec $ "hg pull --update --rebase" ++ upstream
                                    ++ " & hg push " ++ branch
                                    ++ " --force"
                                return (True, True)    -- Sync
                            else return (True, False)  -- TODO: clone
                           else  return (False, False) -- directory exists but it's not a hg

        in (return (False, False)) >>= (chk gitX)
                                   >>= (chk hgX)

setEnv :: String -> IO()
setEnv env = exec $ if | os `elem` ["win32", "mingw32"] -> "set " ++ env
                       | os `elem` ["darwin", "cygwin32"] -> "export " ++ env
                       | otherwise -> "export " ++ env

gentooSync :: String -> Maybe String -> IO()
gentooSync path jobs = do
    j <-  case jobs of
            Just jj -> return jj
            Nothing -> if | os `elem` ["win32", "mingw32"] -> return "2"
                          | otherwise -> readProcess "nproc" [] []
    putStrLn "updating..."
    asyncReactive (exc path $ " cvs update "
                    ++ " & egencache --update --repo=gentoo --portdir=" ++ path
                    ++ " --jobs=" ++ j)

gpull :: String -> String -> IO()
gpull path branch = doesDirectoryExist path 
                    >>= (ifSo $ exc path $ "git pull origin " ++ branch)

gclone :: String -> String -> IO()
gclone path project =
    doesDirectoryExist path >>= \dirExist -> 
        if dirExist then putStrLn $ "directory already exist"
                    else exec $ "git clone " ++ project ++ " " ++ path
