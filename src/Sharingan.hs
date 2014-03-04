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

import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Eternal

import System.FilePath(takeDirectory, (</>))

import Data.List (isInfixOf)

main :: IO ()
main = do user      <- getAppUserDataDirectory "sharingan.lock"
          locked    <- doesFileExist user
          let run = withFile user WriteMode do_program
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
    optJobs :: String,  optSync :: String,
    optG    :: Bool,    optFast :: String -> String -> IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optJobs    = "2",   optG       = False,
    optSync    = "",    optFast    = go False
  }

do_program :: Handle -> IO ()
do_program _ = do args <- getArgs
                  let ( actions, _, _ ) = getOpt RequireOrder options args
                  opts <- foldl (>>=) (return defaultOptions) actions
                  let Options { optSync = sync,
                                optFast = run,
                                optJobs = jobs,
                                optG    = g } = opts
                  if g  then genSync  jobs
                        else run sync jobs

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showV) "Display Version",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    Option ['D'] ["depot"]   (NoArg getDepot) "Get Google depot tools with git and python",
    Option ['l'] ["list"]    (NoArg list) "List repositories",
    Option ['g'] ["gentoo"]  (NoArg genS) "Synchronize cvs portagee tree Gentoo x86",
    Option ['j'] ["jobs"]    (ReqArg getJ "STRING") "Maximum parallel jobs",
    Option ['s'] ["sync"]    (ReqArg gets "STRING") "sync single repository",
    Option ['f'] ["fast"]    (NoArg fastReinstall) "fast sync, don't process .sharingan.yml files"
  ]

genSync    ::   String -> IO()
genSync j  =    gentooSync "/home/gentoo-x86" j >> exitWith ExitSuccess

list       ::   Options -> IO Options
getDepot   ::   Options -> IO Options
showV      ::   Options -> IO Options
showHelp   ::   Options -> IO Options
genS       ::   Options -> IO Options

getDepot _ =    depot_tools                     >> exitWith ExitSuccess
showV _    =    printf "sharingan 0.0.1"        >> exitWith ExitSuccess
showHelp _ = do putStrLn $ usageInfo "Usage: sharingan [optional things]" options
                exitWith ExitSuccess

getJ            ::   String -> Options -> IO Options
gets            ::   String -> Options -> IO Options
fastReinstall   ::   Options -> IO Options

genS opt            = return opt { optG = True }
getJ arg opt        = return opt { optJobs = arg }
gets arg opt        = return opt { optSync = arg }
fastReinstall opt   = return opt { optFast = go True }

lyricsBracket :: IO() -> IO()
lyricsBracket = bracket_
 ( do putStrLn "             Heaven Conceal                                                                 "
      putStrLn "_________________________________________________________________________________________   "
   )( do putStrLn "   Done                                                                                  "
         putStrLn "_________________________________________________________________________________________"
    )

list _ = (</> "sharingan.yml")
  <$> takeDirectory
  <$> getExecutablePath >>= \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        forM_ rsdata $ \repo -> do
            let loc = location repo
            forM_ (branches repo) $ printf " * %s <> %s\n" loc
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

go :: Bool -> String -> String -> IO()
go fast sync _ = (</> "sharingan.yml")                {- lens:                           -}
  <$> takeDirectory                                   {- (& filename .~ "sharingan.yml") -}
  <$> getExecutablePath >>= \ymlx ->
    let ymlprocess = ifSo $ lyricsBracket $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        forM_ rsdata $ \repo ->
            let loc = location repo
            in when (sync == "" || isInfixOf sync loc)
                $ forM_ (branches repo) $ \branch ->
                    printf " * %s <> %s\n" loc branch
                    >>  let eye = ifSo 
                               $ when (not fast)
                               $ let shx = loc </> ".sharingan.yml"
                                     vs = ifSo $ do syncDatax <- yDecode shx :: IO Sharingan
                                                    --TODO: language defaults
                                                    forM_ (env syncDatax) $ setEnv
                                                    forM_ (before_install syncDatax) $ exc loc
                                                    forM_ (install syncDatax) $ exc loc
                                                    forM_ (script syncDatax) $ exc loc
                                 in doesFileExist shx >>= vs
                        in rebasefork loc branch <| upstream repo >>= eye
                    >>  putStrLn <| replicate 89 '_'
    in doesFileExist ymlx >>= ymlprocess
