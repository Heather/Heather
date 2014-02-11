module Shell
  ( exec,
    exc,
    gpull,
    gclone,
    rebasefork,
  ) where

import System.IO
import System.Exit
import System.Directory
import System.Process
import System.FilePath((</>))

import Control.Monad
{----------------------------------------------------------------------------------------}
exec :: [Char] -> IO()
exec args = do
    pid <- runCommand args
    waitForProcess pid >> return ()
{----------------------------------------------------------------------------------------}
exc :: [Char] -> [Char] -> IO()
exc path args = exec $ "cd " ++ path ++ " & " ++ args
{----------------------------------------------------------------------------------------}
rebasefork :: [Char] -> [Char] -> [Char] -> IO()
rebasefork path branch upstream = do
    doesDirectoryExist path >>= (flip when
        $ exc path $ "git checkout " ++ branch   
                ++ " & git rebase --abort & git pull origin " ++ branch
                ++ " & git fetch " ++ upstream
                ++ " & git pull --rebase " ++ upstream
                ++ " & git push --force origin " ++ branch)
{----------------------------------------------------------------------------------------}
gpull :: [Char] -> [Char] -> IO()
gpull path branch =
    doesDirectoryExist path >>= (flip when
        $ exc path $ "git pull origin " ++ branch)
{----------------------------------------------------------------------------------------}
gclone :: [Char] -> [Char] -> IO()
gclone path project =
    doesDirectoryExist path >>= \dirExist -> 
        if dirExist then putStrLn $ "directory already exist"
                    else exec $ "git clone " ++ project ++ " " ++ path
{----------------------------------------------------------------------------------------}
