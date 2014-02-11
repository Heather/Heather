module Shell
  ( exec,
    exc,
    svnup,
    svnclone,
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
        $ do exc path $ "git checkout " ++ branch   
                ++ " & git rebase --abort & git pull origin " ++ branch
                ++ " & git fetch " ++ upstream
                ++ " & git pull --rebase " ++ upstream
                ++ " & git push --force origin " ++ branch)
{----------------------------------------------------------------------------------------}
svnup :: [Char] -> IO()
svnup path =
    doesDirectoryExist path >>= \dirExist -> 
        if dirExist then exc path "svn update"
                    else putStrLn $ "directory does not exist"
{----------------------------------------------------------------------------------------}
svnclone :: [Char] -> [Char] -> [Char] -> IO()
svnclone ex path project =
    doesDirectoryExist path >>= \dirExist -> 
        if dirExist then putStrLn $ "directory already exist"
                    else exc ex $ "svn co " ++ project ++ " " ++ path
{----------------------------------------------------------------------------------------}
gpull :: [Char] -> [Char] -> IO()
gpull path branch =
    doesDirectoryExist path >>= \dirExist -> 
        if dirExist then putStrLn $ "directory already exist"
                    else exc path $ "git pull origin " ++ branch
{----------------------------------------------------------------------------------------}
gclone :: [Char] -> [Char] -> IO()
gclone path project =
    doesDirectoryExist path >>= \dirExist -> 
        if dirExist then putStrLn $ "directory already exist"
                    else exec $ "git clone " ++ project ++ " " ++ path
{----------------------------------------------------------------------------------------}
