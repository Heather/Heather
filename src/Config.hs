{-# LANGUAGE MultiWayIf #-}

module Config
  ( getConfig
  , getDefaultsConfig
  
  , processChecks
  , processDefaultsChecks
  
  , withConfig
  , withDefaultsConfig
  
  , config
  , defaultsConfig
  
  , getA, getAC
  , getD, getDC
  
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

getDefaultsConfig :: IO FilePath
getDefaultsConfig =
    if | os `elem` ["win32", "mingw32", "cygwin32"] -> (</> "sharinganDefaults.yml") 
                                                        <$> takeDirectory 
                                                        <$> getExecutablePath
       | otherwise -> return "/etc/sharinganDefaults.yml"

processChecks :: FilePath -> IO()
processChecks cfg = 
  let generate cfg = ifNot $ let nothing = [] :: [Repository]
                             in yEncode cfg nothing
  in doesFileExist cfg >>= generate cfg

processDefaultsChecks :: FilePath -> IO()
processDefaultsChecks cfg = 
  let generate cfg = ifNot $ let nothing = (Defaults Nothing)
                             in yEncode cfg nothing
  in doesFileExist cfg >>= generate cfg

withConfig foo = liftM2 (>>) processChecks foo =<< getConfig
withDefaultsConfig foo = liftM2 (>>) processDefaultsChecks foo =<< getDefaultsConfig

config :: Options -> IO Options
config _ = do
    editor <- getEnv "EDITOR"
    withConfig $ \ymlx ->
        exec $ editor ++ " " ++ ymlx
    exitWith ExitSuccess

defaultsConfig :: Options -> IO Options
defaultsConfig _ = do
    editor <- getEnv "EDITOR"
    withDefaultsConfig $ \ymlx ->
        exec $ editor ++ " " ++ ymlx
    exitWith ExitSuccess

getA :: String -> Options -> IO Options
getA arg _ = -- Add new stuff to sync
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        let new = (Repository arg 
                              ["master"] "upstream master"
                              Nothing Nothing Nothing Nothing Nothing)
        if (elem new rsdata)
            then putStrLn "this repository is already in sync"
            else let newdata = new : rsdata
                 in yEncode ymlx newdata
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

getAC :: Options -> IO Options
getAC o = do cdir <- getCurrentDirectory
             getA cdir o

getD :: String -> Options -> IO Options
getD arg _ = -- Remove stuff from sync
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata  <- yDecode ymlx :: IO [Repository]
        let iio x = isInfixOf arg $ location x
            findx = find iio rsdata
        case findx of
            Just fnd -> do yEncode ymlx $ filter (/= fnd) rsdata
                           putStrLn $ (location fnd) ++ " is removed"
            Nothing  -> putStrLn $ arg ++ " repo not found"
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

getDC :: Options -> IO Options
getDC o = do cdir <- getCurrentDirectory
             getD cdir o

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
