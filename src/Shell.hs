{-# LANGUAGE MultiWayIf #-}

module Shell
  ( exec,
    exc,
    setEnv,
    gpull,
    gclone,
    rebasefork,
    gentooSync
  ) where

import Data.List.Split

import System.Info (os)
import System.Directory
import System.FilePath((</>))

import System.Process

import Control.Monad
import Control.Eternal

rebasefork :: [Char] -> [Char] -> [Char] -> IO Bool
rebasefork path branch upstream =
    doesDirectoryExist path >>= \dirExist ->
        let chk foo previous = if previous
                then return True
                else foo

            gitX = doesDirectoryExist <| path </> ".git" >>= \git ->
                    if git then if dirExist
                            then setCurrentDirectory path >> do
                                    exec $ "git checkout " ++ branch
                                        ++ " & git reset --hard"
                                        ++ " & git rebase --abort"
                                    loc <- readProcess "git" ["log", "-n", "1", "--pretty=format:%H"] []
                                    rem <- readProcess "git" (["ls-remote"] ++ (splitOn " " upstream)) []
                                    let remote = (splitOn "\t" rem) !! 0
                                    putStrLn $ "Local: "  ++ loc
                                    putStrLn $ "Remote: " ++ remote
                                    if  remote == loc
                                        then do putStrLn $ path ++ " is up to date"
                                                return True -- repository is up to date
                                        else do exec $ "git pull origin "             ++ branch
                                                     ++ " & git fetch "               ++ upstream
                                                     ++ " & git pull --rebase "       ++ upstream
                                                     ++ " & git push --force origin " ++ branch
                                                return True -- Sync
                            else return True  -- TODO: clone
                           else  return False -- directory exists but it's not a git

            hgX = doesDirectoryExist <| path </> ".hg" >>= \hg ->
                    if hg then if dirExist
                            then setCurrentDirectory path >> do
                                exec $ "hg pull --update --rebase" ++ upstream
                                    ++ " & hg push " ++ branch
                                    ++ " --force"
                                return True   -- Sync
                            else return True  -- TODO: clone
                           else  return False -- directory exists but it's not a hg
                        
        in (return False) >>= chk gitX
                          >>= chk hgX

setEnv :: [Char] -> IO()
setEnv env = exec eset
             where eset = if | os `elem` ["win32", "mingw32"] -> "set " ++ env
                             | os `elem` ["darwin"] -> "export " ++ env
                             | otherwise -> "export " ++ env -- "cygwin32"

gentooSync :: [Char] -> [Char] -> IO()
gentooSync path jobs = exc path $ " cvs update "
            ++ " & egencache --update --repo=gentoo --portdir=" ++ path
            ++ " --jobs="                                       ++ jobs

gpull :: [Char] -> [Char] -> IO()
gpull path branch =
    doesDirectoryExist path >>= (flip when
        $ exc path $ "git pull origin " ++ branch)

gclone :: [Char] -> [Char] -> IO()
gclone path project =
    doesDirectoryExist path >>= \dirExist -> 
        if dirExist then putStrLn $ "directory already exist"
                    else exec $ "git clone " ++ project ++ " " ++ path
