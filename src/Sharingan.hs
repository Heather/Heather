{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

import Yaml
import Tools
import SharinganProcess

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

import System.Info (os)
import System.FilePath(takeDirectory, (</>))

import Data.List (isInfixOf)
import Data.List.Split

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
    optJobs :: String,  optSync :: String,  optInteractive  :: Bool,
    optG    :: Bool,    optFast :: Bool -> String -> String -> IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optJobs    = "2",   optG       = False,     optInteractive = False,
    optSync    = "",    optFast    = go False
  }

do_program :: Handle -> IO ()
do_program _ = do args <- getArgs
                  let ( actions, _, _ ) = getOpt RequireOrder options args
                  opts <- foldl (>>=) (return defaultOptions) actions
                  let Options { optSync = sync,
                                optFast = run,
                                optJobs = jobs,
                                optG    = g,
                                optInteractive = i } = opts
                  if g  then genSync jobs
                        else run i sync jobs

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showV) "Display Version",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    Option ['D'] ["depot"]   (NoArg getDepot) "Get Google depot tools with git and python",
    Option ['l'] ["list"]    (NoArg list) "List repositories",
    Option ['a'] ["add"]     (ReqArg getA "STRING") "Add repository",
    Option ['d'] ["delete"]  (ReqArg getD "STRING") "Delete repository",
    Option ['g'] ["gentoo"]  (NoArg genS) "Synchronize cvs portagee tree Gentoo x86",
    Option ['j'] ["jobs"]    (ReqArg getJ "STRING") "Maximum parallel jobs",
    Option ['s'] ["sync"]    (ReqArg gets "STRING") "sync single repository",
    Option ['f'] ["fast"]    (NoArg fastReinstall) "fast sync, don't process .sharingan.yml files",
    Option ['i'] ["interactive"] (NoArg interactive) "trying guess what to do for each repository"
  ]

genSync    ::   String -> IO()
genSync j  =    gentooSync "/home/gentoo-x86" j >> exitWith ExitSuccess

list        ::   Options -> IO Options
getDepot    ::   Options -> IO Options
showV       ::   Options -> IO Options
showHelp    ::   Options -> IO Options
genS        ::   Options -> IO Options
interactive ::   Options -> IO Options

showV _    =    printf "sharingan 0.0.2"        >> exitWith ExitSuccess
showHelp _ = do putStrLn $ usageInfo "Usage: sharingan [optional things]" options
                exitWith ExitSuccess
getDepot _ = do if (os `elem` ["win32", "mingw32", "cygwin32"]) 
                  then depot_tools
                  else putStrLn "this option is Win-only"
                exitWith ExitSuccess

getA            ::   String -> Options -> IO Options
getD            ::   String -> Options -> IO Options
getJ            ::   String -> Options -> IO Options
gets            ::   String -> Options -> IO Options
fastReinstall   ::   Options -> IO Options

genS opt            = return opt { optG = True }
interactive opt     = return opt { optInteractive = True }
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
  
getA arg _ = -- Add new stuff to sync
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata  <- yDecode ymlx :: IO [Repository]
        let new = (Repository arg 
                              ["master"] 
                              "upstream master")
        if (elem new rsdata)
            then putStrLn "this repository is already in sync"
            else let newdata = new : rsdata
                 in yEncode ymlx newdata
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

getD arg _ = -- Remove stuff from sync
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata  <- yDecode ymlx :: IO [Repository]
        let fd = filter notD rsdata
                        where notD x = not $ isInfixOf arg
                                           $ location x
        yEncode ymlx fd
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

list _ =
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        forM_ rsdata $ \repo -> do
            let loc = location repo
            forM_ (branches repo) $ printf " * %s <> %s\n" loc
    in doesFileExist ymlx >>= ymlprocess 
                          >> exitWith ExitSuccess

go :: Bool -> Bool -> String -> String -> IO()
go fast intera sync _ =
  withConfig $ \ymlx ->                           
    let ymlprocess = ifSo $ lyricsBracket $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        forM_ rsdata $ \repo ->
            let loc = location repo
                up  = upstream repo
                br  = branches repo
            in when (sync == "" || isInfixOf sync loc)
                $ forM_ br $ \branch ->
                    printf " * %s <> %s\n" loc branch
                    >> let eye = ifSo 
                            $ when (not fast)
                            $ let shx = loc </> ".sharingan.yml"
                              in doesFileExist shx  >>= sharingan intera shx loc
                        in rebasefork loc branch up >>= eye
                    >>  putStrLn <| replicate 89 '_'
    in doesFileExist ymlx >>= ymlprocess
