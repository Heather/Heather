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

  , module Yaml
  ) where

import Yaml

import System.Directory
import System.IO
import System.Info (os)
import System.Environment.Executable ( getExecutablePath )
import System.Exit
import System.FilePath(takeDirectory, (</>))
import System.Environment( getEnv )

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

config :: IO()
config = do editor <- getEnv "EDITOR"
            withConfig $ \ymlx ->
                exec $ editor ++ " " ++ ymlx
            exitWith ExitSuccess

defaultsConfig :: IO ()
defaultsConfig = do editor <- getEnv "EDITOR"
                    withDefaultsConfig $ \ymlx ->
                        exec $ editor ++ " " ++ ymlx
                    exitWith ExitSuccess

{- TODO : Calc hash right here
readProcess "git" ["log", "-n", "1"
                  , "--pretty=format:%H"
                  ] []
-}
getA :: String -> IO ()
getA arg = -- Add new stuff to sync
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        let new = (Repository arg "rebase" -- rebase as default task
                              ["master"] "upstream master"
                              Nothing Nothing Nothing Nothing Nothing)
        if (elem new rsdata)
            then putStrLn "this repository is already in sync"
            else let newdata = new : rsdata
                 in yEncode ymlx newdata
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

getAC :: [String] -> IO ()
getAC []     = getCurrentDirectory >>= getA
getAC (x:[]) = getA x
getAC (x:xs) = do getA x
                  getAC xs

getD :: String -> IO ()
getD arg = -- Remove stuff from sync
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

getDC :: [String] -> IO ()
getDC []     = getCurrentDirectory >>= getD
getDC (x:[]) = getD x
getDC (x:xs) = do getD x
                  getDC xs

enable :: Bool -> String -> IO ()
enable en arg =
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
