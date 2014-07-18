{-# LANGUAGE MultiWayIf, LambdaCase, OverloadedStrings #-}

import Despair

import Yaml
import Tools
import SharinganProcess

--TODO: Use progress bar
--import ProgressBar

import Text.Printf

import System.Environment( getArgs, getEnv )
import System.Directory
import System.Exit
import System.Console.GetOpt
import System.IO

import System.Environment.Executable ( getExecutablePath )

import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Eternal
import Control.Concurrent

import System.Info (os)
import System.FilePath(takeDirectory, (</>))

import Data.Maybe
import Data.List
import Data.List.Split

version :: String
version = "0.0.6"

main :: IO ()
main = do args <- getArgs
          let ( actions, _, _ ) = getOpt RequireOrder options args
          opts <- foldl (>>=) (return defaultOptions) actions
          let Options { optSync = sync,     optForce = f,
                        optFast = start,    optJobs = jobs, optUnsafe = unsafe,
                        optG    = g,        optInteractive = i } = opts
          user      <- getAppUserDataDirectory "sharingan.lock"
          locked    <- doesFileExist user
          let gogo = if g then genSync jobs
                          else start unsafe i f sync jobs
              run = withFile user WriteMode (do_program gogo)
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
    optG    :: Bool,    optForce :: Bool, optUnsafe :: Bool,
    optFast :: Bool -> Bool -> Bool -> String -> String -> IO()
  }

defaultOptions :: Options
defaultOptions = Options {
    optJobs    = "2",   optG       = False,     optInteractive = False,
    optSync    = "",    optForce   = False,     optUnsafe = False,
    optFast    = go False
  }

do_program :: IO() -> Handle -> IO()
do_program gogo _ = gogo

gOptions :: [OptDescr (Options -> IO Options)]
gOptions = [
    Option ['v'] ["version"] (NoArg showV) "Display Version",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    
    Option []    ["make"]    (NoArg mkSharingan) "Create .sharingan.yml template",
    Option []    ["config"]  (NoArg config) "Edit .sharingan.yml config file",
    
    Option ['l'] ["list"]    (OptArg list "STRING") "List repositories",
    Option ['a'] ["add"]     (ReqArg getA "STRING") "Add repository",
    Option ['d'] ["delete"]  (ReqArg getD "STRING") "Delete repository",
    Option ['j'] ["jobs"]    (ReqArg getJ "STRING") "Maximum parallel jobs",
    Option ['s'] ["sync"]    (ReqArg gets "STRING") "sync single repository",
    
    Option ['u'] ["unsafe"]   (NoArg runUnsafe) "do not process reset before sync",
    Option ['q'] ["quick"]    (NoArg fastReinstall) "quick sync, don't process .sharingan.yml files",
    Option ['f'] ["force"]    (NoArg forceReinstall) "force process .sharingan.yml files",
    Option ['i'] ["interactive"] (NoArg interactive) "trying guess what to do for each repository"
  ]
  
winOptions :: [OptDescr (Options -> IO Options)]
winOptions = [
    Option ['D'] ["depot"]   (NoArg getDepot) "Get Google depot tools with git and python"
  ]
  
nixOptions :: [OptDescr (Options -> IO Options)]
nixOptions = [
    Option ['g'] ["gentoo"]  (NoArg genS) "Synchronize cvs portagee tree Gentoo x86"
  ]
  
options :: [OptDescr (Options -> IO Options)]
options = gOptions ++ if | os `elem` ["win32", "mingw32", "cygwin32"] -> winOptions
                         | otherwise -> nixOptions

genSync    ::   String -> IO()
genSync j  =    gentooSync "/home/gentoo-x86" j >> exitWith ExitSuccess

getDepot        ::   Options -> IO Options
mkSharingan     ::   Options -> IO Options
config          ::   Options -> IO Options
showV           ::   Options -> IO Options
showHelp        ::   Options -> IO Options
genS            ::   Options -> IO Options
interactive     ::   Options -> IO Options
fastReinstall   ::   Options -> IO Options
forceReinstall  ::   Options -> IO Options
runUnsafe       ::   Options -> IO Options

printver  :: IO ()
printver   = putStrLn $ "Sharingan " ++ version ++ " " ++ (show os)
showV _    = printver >> exitWith ExitSuccess
showHelp _ = do printver
                putStrLn $ usageInfo "Usage: sharingan [optional things]" options
                exitWith ExitSuccess
getDepot _ = do depot_tools
                exitWith ExitSuccess

list            ::   Maybe String -> Options -> IO Options
getA            ::   String -> Options -> IO Options
getD            ::   String -> Options -> IO Options
getJ            ::   String -> Options -> IO Options
gets            ::   String -> Options -> IO Options

genS opt            = return opt { optG = True }
interactive opt     = return opt { optInteractive = True }
getJ arg opt        = return opt { optJobs = arg }
gets arg opt        = return opt { optSync = arg }
forceReinstall opt  = return opt { optForce = True }
runUnsafe opt       = return opt { optUnsafe = True }
fastReinstall opt   = return opt { optFast  = go True }

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
                              ["master"] "upstream master"
                              Nothing)
        if (elem new rsdata)
            then putStrLn "this repository is already in sync"
            else let newdata = new : rsdata
                 in yEncode ymlx newdata
    in doesFileExist ymlx >>= ymlprocess 
                          >>  exitWith ExitSuccess

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

list arg _ =
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
         rsdata <- yDecode ymlx :: IO [Repository]
         let rdd = case arg of
                    Just s  -> filter (\r -> isInfixOf s (location r)) rsdata
                    Nothing -> rsdata
         forM_ rdd $ \repo ->
            let loc  = location repo
                sstr = " - " ++ loc ++ " : "
                empt = replicate (length sstr) ' '
                brx  = (branches repo)
            in if (length brx) == 0
                then printf " - %s\n" loc
                else do printf "%s%s\n" sstr $ head brx
                        forM_ (drop 1 brx) $ printf "%s%s\n" empt
    in doesFileExist ymlx >>= ymlprocess 
                          >>  exitWith ExitSuccess

mkSharingan _ = -- Create .sharingan.yml template
  let langM = Just "haskell"
      envM  = Just []
      biM   = Just []
      iM    = Just []
      new   = (Sharingan langM envM biM iM ["cabal install"])
  in yEncode ".sharingan.yml" new >> exitWith ExitSuccess

config _ = do
    editor <- getEnv "EDITOR"
    withConfig $ \ymlx ->
        exec $ editor ++ " " ++ ymlx
    exitWith ExitSuccess

go :: Bool -> Bool -> Bool -> Bool -> String -> String -> IO()
go fast unsafe intera force sync _ =
  withConfig $ \ymlx ->                           
    let ymlprocess = ifSo $ despair $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        forM_ rsdata $ \repo ->
            let loc = location repo
                up  = splitOn " " $ upstream repo
                br  = branches repo
                ps  = post_rebuild repo
                bsync = if (length up) > 1
                            then up !! 1 `elem` br
                            else False
                u b = do printf " - %s : %s\n" loc b
                         rebasefork loc b up unsafe bsync
                eye r = when ((r || force) && (not fast))
                            $ do let shx = loc </> ".sharingan.yml"
                                 doesFileExist shx >>= sharingan intera shx loc
                                 when (isJust ps) $ forM_ (fromJust ps) $ \psc ->
                                                        let pshx = psc </> ".sharingan.yml"
                                                        in doesFileExist pshx
                                                            >>= sharingan intera pshx psc
            in when (sync == "" || isInfixOf sync loc)
                $ do forM_ (tails br)
                        $ \case x:[] -> u x >>= eye
                                x:xs -> u x >>= (\_ -> return ())
                                []   -> return ()
                     putStrLn <| replicate 89 '_'

    in doesFileExist ymlx >>= ymlprocess
