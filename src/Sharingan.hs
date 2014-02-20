{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
{----------------------------------------------------------------------------------------}
import Yaml
import Shell
import Tools
{----------------------------------------------------------------------------------------}
import Text.Printf

import System.Environment( getArgs )
import System.Directory
import System.Exit
import System.Console.GetOpt
import System.IO

import System.Environment.Executable ( getExecutablePath )

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Eternal

import System.FilePath(takeDirectory, (</>))

import Data.List (isInfixOf)

main :: IO ()
main = do user      <- getAppUserDataDirectory "sharingan.lock"
          locked    <- doesFileExist user
          let run = myThreadId >>= \t -> withFile user WriteMode (do_program t)
                                           `finally` removeFile user
          if locked then do
                        putStrLn "There is already one instance of this program running."
                        putStrLn "Remove lock and start application? (Y/N)"
                        hFlush stdout
                        str <- getLine
                        if | str `elem` ["Y", "y"] -> run
                           | otherwise             -> return ()
                      else run

data Options = Options  {
    optSync :: String,
    optFast :: String -> IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optSync    = "",
    optFast    = go False
  }

do_program :: ThreadId -> Handle -> IO ()
do_program t h = let s = "Locked by thread: " ++ show t
                 in do  putStrLn s
                        hPutStr h s
                        args <- getArgs
                        let ( actions, _, _ ) = getOpt RequireOrder options args
                        opts <- foldl (>>=) (return defaultOptions) actions
                        let Options { optSync = sync,
                                      optFast = run } = opts
                        run sync

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showV) "Display Version",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    Option ['D'] ["depot"]   (NoArg getDepot) "Get Google depot tools with git and python",
    Option ['g'] ["gentoo"]  (NoArg genSync) "Synchronize cvs portagee tree Gentoo x86",
    Option ['s'] ["sync"]    (ReqArg gets "STRING") "sync single repository",
    Option ['f'] ["fast"]    (NoArg fastReinstall) "fast sync, don't process .sharingan.yml files"
  ]

getDepot   ::   Options -> IO Options
genSync    ::   Options -> IO Options
showV      ::   Options -> IO Options
showHelp   ::   Options -> IO Options

getDepot _ =    depot_tools                     >> exitWith ExitSuccess
genSync _  =    gentooSync "/home/gentoo-x86" 2 >> exitWith ExitSuccess
showV _    =    printf "sharingan 0.0.1"        >> exitWith ExitSuccess
showHelp _ = do putStrLn $ usageInfo "Usage: sharingan [optional things]" options
                exitWith ExitSuccess

gets            ::   String -> Options -> IO Options
fastReinstall   ::   Options -> IO Options

gets arg opt        = return opt { optSync = arg }
fastReinstall opt   = return opt { optFast = go True }

lyricsBracket :: IO() -> IO()
lyricsBracket = bracket_
 ( do
    putStrLn "____________________________________________________________________________________________"
    putStrLn "                    And who the hell do you think I've become?                              "
    putStrLn "  Like the person inside, I've been opening up.                                             "
    putStrLn "                                                     I'm onto you. (I'm onto you.)          "
    putStrLn "____________________________________________________________________________________________"
 )(do
    putStrLn "     Cut out your tongue and feed it to the liars.                                          "
    putStrLn "                  Black hearts shed light on dying words.                                   "
    putStrLn "                                                                                            "
    putStrLn "                                                            I wanna feel you burn.          "
    putStrLn "____________________________________________________________________________________________"
 )

go :: Bool -> String -> IO()
go fast sync = (</> "sharingan.yml")                   {- lens:                           -}
  <$> takeDirectory                                 {- (& filename .~ "sharingan.yml") -}
  <$> getExecutablePath >>= \ymlx ->
    let ymlprocess = ifSo $ lyricsBracket $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        forM_ rsdata $ \repo ->
            let loc = location repo
            in when (sync == "" || isInfixOf sync loc)
                $ forM_ (branches repo) $ \branch -> do
                    printf " * %s <> %s\n" loc branch
                    >>  let eye = ifSo 
                               $ when (not fast)
                               $ let shx = loc </> ".sharingan.yml"
                                     vs = ifSo $ do syncDatax <- yDecode shx :: IO Sharingan                  
                                                    forM_ (script syncDatax) $ exc loc
                                 in doesFileExist shx >>= vs
                        in rebasefork loc branch <| upstream repo >>= eye
                    >>  putStrLn <| replicate 92 '_'
    in doesFileExist ymlx >>= ymlprocess
