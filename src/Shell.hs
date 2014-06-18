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

import ProgressBar

import Data.Char
import Data.List
import Data.List.Split

import System.IO
import System.Info (os)
import System.Directory
import System.FilePath((</>))

import System.Process

import Control.Monad
import Control.Eternal
import Control.Concurrent

trim xs = dropSpaceTail "" $ dropWhile isSpace xs
dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

rebasefork :: [Char] -> [Char] -> [[Char]] -> Bool -> Bool -> IO Bool
rebasefork path branch up unsafe sync =
    let upstream = intercalate " " up
    in doesDirectoryExist path >>= \dirExist ->
        let chk foo previous = if previous
                then return True
                else foo

            gitX = doesDirectoryExist <| path </> ".git" >>= \git ->
                    if git then if dirExist
                            then setCurrentDirectory path >> do
                                    when (not unsafe) 
                                        $ exec $ "git checkout " ++ branch
                                                                 ++ " & git reset --hard"
                                                                 ++ " & git rebase --abort"
                                    loc <- if (length up) > 1 && sync
                                            then readProcess "git" [ "merge-base"
                                                                   , up !! 1
                                                                   , "origin/" ++ branch
                                                                   ] []
                                            else readProcess "git" ["log", "-n", "1"
                                                                   , "--pretty=format:%H"
                                                                   ] []
                                    rem <- readProcess "git" (["ls-remote"] ++ up) []
                                    let remote = (splitOn "\t" rem) !! 0
                                        local  = trim loc
                                    putStrLn $ "Local: "  ++ local
                                    putStrLn $ "Remote: " ++ remote
                                    if  remote == local
                                        then do putStrLn $ path ++ " is up to date"
                                                return False -- repository is up to date
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
gentooSync path jobs = do
    putStrLn "updating..."
    putProgress $ drawProgressBar 40 0 ++ " " ++ drawPercentage 0
    forkIO $ exc path $ " cvs update "
            ++ " & egencache --update --repo=gentoo --portdir=" ++ path
            ++ " --jobs="
    putProgress $ drawProgressBar 40 100 ++ " " ++ drawPercentage 100
    hPutChar stderr '\n'

gpull :: [Char] -> [Char] -> IO()
gpull path branch =
    doesDirectoryExist path >>= (flip when
        $ exc path $ "git pull origin " ++ branch)

gclone :: [Char] -> [Char] -> IO()
gclone path project =
    doesDirectoryExist path >>= \dirExist -> 
        if dirExist then putStrLn $ "directory already exist"
                    else exec $ "git clone " ++ project ++ " " ++ path
