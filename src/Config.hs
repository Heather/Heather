{-# LANGUAGE MultiWayIf #-}

module Config
  ( getConfig
  , processChecks
  , withConfig
  , config
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
import System.Environment( getEnv )

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

config :: Options -> IO Options
config _ = do
    editor <- getEnv "EDITOR"
    withConfig $ \ymlx ->
        exec $ editor ++ " " ++ ymlx
    exitWith ExitSuccess

enable :: Bool -> String -> Options -> IO Options
enable en arg _ =
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        let fr x = if isInfixOf arg $ location x
                    then x { enabled = Just en }
                    else x
        yEncode ymlx $ map fr rsdata
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

hashupdate :: String -> String -> IO ()
hashupdate hsh rep =
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        let fr x = if rep == location x
                    then x { hash = Just hsh }
                    else x
        yEncode ymlx $ map fr rsdata
    in doesFileExist ymlx >>= ymlprocess
