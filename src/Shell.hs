{-# LANGUAGE MultiWayIf, LambdaCase, CPP #-}

module Shell
  ( amaterasu
  , exec
  , exc
  , gentooSync
  , setEnv
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

setEnv :: String -> IO()
setEnv env = exec $ if | os `elem` ["win32", "mingw32"] -> "set " ++ env
                       | os `elem` ["darwin", "cygwin32"] -> "export " ++ env
                       | otherwise -> "export " ++ env

amaterasu :: String -> String -> String -> [String] -> Bool -> Bool -> Maybe String -> Bool -> IO (Bool, Bool)
amaterasu "rebase"  = rebasefork
amaterasu "pull"    = pull
amaterasu custom    = \path _ _ unsafe processClean _ _ -> do
    doesDirectoryExist <| path </> ".git" >>= \git ->
        when git $ do
            when processClean $ exec "git clean -xdf"
            when (not unsafe) $ exec "git reset --hard & git rebase --abort"
    doesDirectoryExist path >>= \dirExist ->
        if dirExist then setCurrentDirectory path >> do
                            exec custom
                            return (True, True)
                    else return (False, False)

pull :: String -> String -> [String] -> Bool -> Bool -> Maybe String -> Bool -> IO (Bool, Bool)
pull path branch _ unsafe processClean rhash sync =
    doesDirectoryExist path >>= \dirExist ->
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
                                        _ -> readProcess "git" ["log", "-n", "1"
                                                               , "--pretty=format:%H"
                                                               ] []
                                let local  = trim loc
                                rlc <- readProcess "git" ["ls-remote", "origin", branch] []
                                lrc <- if isNothing rhash then readProcess "git" ["log", "-n", "1"
                                                                                 , "--pretty=format:%H"
                                                                                 ] []
                                                          else return loc
                                let remloc = (splitOn "\t" rlc) !! 0
                                    locloc = trim lrc
                                putStrLn $ "Origin: " ++ remloc
                                putStrLn $ "Local: "  ++ locloc
                                if (remloc /= locloc) 
                                    then do exec $ "git pull origin " ++ branch
                                            hashupdate remloc path
                                            return (True, True)
                                    else return (True, False)
                            else return (True, True)
                           else  return (False, False)

            hgX = doesDirectoryExist <| path </> ".hg" >>= \hg ->
                    if hg then if dirExist
                            then setCurrentDirectory path >> do
                                exec $ "hg pull --update"
                                return (True, True)    -- Sync
                            else return (True, False)  -- TODO: clone
                           else  return (False, False) -- directory exists but it's not a hg

        in (return (False, False)) >>= (chk gitX)
                                   >>= (chk hgX)

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
                                            whe (remloc /= remote) 
                                                  $ "git pull --rebase "          ++ upstream
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

gentooSync :: String -> Int -> IO()
gentooSync path jobs = do
#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
    j <- if jobs == 0 then readProcess "nproc" [] []
                      else return $ show jobs
    putStrLn "updating..."
    asyncReactive (exc path $ " cvs update "
                    ++ " & egencache --update --repo=gentoo --portdir=" ++ path
                    ++ " --jobs=" ++ j)
#else
    return ()
#endif
