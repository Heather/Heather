{-# LANGUAGE MultiWayIf #-}

module Config
  ( getConfig
  , processChecks
  , withConfig
  , enable
  , hashupdate
  ) where

import Yaml
import Model

import System.Directory
import System.IO
import System.Info (os)
import System.Environment.Executable ( getExecutablePath )
import System.Console.GetOpt
import System.Exit
import System.FilePath(takeDirectory, (</>))

import Control.Monad
import Control.Applicative
import Control.Eternal

import Data.List

getConfig :: IO FilePath
getConfig =
    if | os `elem` ["win32", "mingw32", "cygwin32"] -> (</> "sharingan.yml") 
                                                        <$> takeDirectory 
                                                        <$> getExecutablePath
       | otherwise -> return "/etc/sharingan.yml"

processChecks :: FilePath -> IO()
processChecks cfg = 
  let generate cfg = ifNot $ let nothing = [] :: [Repository]
                             in yEncode cfg nothing
  in doesFileExist cfg >>= generate cfg

withConfig foo = liftM2 (>>) processChecks foo =<< getConfig

enable :: Bool -> String -> Options -> IO Options
enable en arg _ =
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        let ed = map enR rsdata
                        where enR x = if isInfixOf arg $ location x
                                            then (Repository (location x)
                                                             (branches x)
                                                             (upstream x)
                                                             (Just en)
                                                             (clean x)
                                                             (post_rebuild x)
                                                             (syncGroup x)
                                                             (hash x))
                                            else x
        yEncode ymlx ed
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

hashupdate :: String -> String -> IO ()
hashupdate hash rep =
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        let ed = map enR rsdata
                        where enR x = if rep == (location x)
                                            then (Repository (location x)
                                                             (branches x)
                                                             (upstream x)
                                                             (enabled x)
                                                             (clean x)
                                                             (post_rebuild x)
                                                             (syncGroup x)
                                                             (Just hash))
                                            else x
        yEncode ymlx ed
    in doesFileExist ymlx >>= ymlprocess