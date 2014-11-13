{-# LANGUAGE MultiWayIf, LambdaCase, OverloadedStrings #-}

import Despair

import Model
import Yaml
import Tools
import Config
import SharinganProcess

import Text.Printf

import System.Environment( getArgs )
import System.Info (os)
import System.Directory
import System.Exit
import System.Console.GetOpt
import System.IO

import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Eternal
import Control.Concurrent

import System.FilePath(takeDirectory, (</>))

import Data.Maybe
import Data.List
import Data.List.Split

version :: String
version = "0.2.6"

main :: IO ()
main = do args <- getArgs
          let ( actions, nonops, _ ) = getOpt RequireOrder options args
          Options { optSync = sync, optSyncGroup = syncGroup
                  , optForce = f,   optFast = start
                  , optJobs = jobs, optUnsafe = unsafe
                  , optG    = g,    optInteractive = i 
                  } <- foldl (>>=) (return defaultOptions) actions
          user      <- getAppUserDataDirectory "sharingan.lock"
          locked    <- doesFileExist user
          let gogo = if g then genSync jobs
                          else start nonops unsafe i f sync syncGroup jobs
              run = withFile user WriteMode (do_program gogo)
                       `finally` removeFile user
          if locked then do
                        putStrLn "There is already one instance of this program running."
                        putStrLn "Remove lock and start application? (Y/N)"
                        hFlush stdout
                        str <- getLine
                        if str `elem` ["Y", "y"] then run
                                                 else return ()
                    else run

defaultOptions :: Options
defaultOptions = Options 
    { optJobs = Nothing,        optG = False
    , optInteractive = False,   optSync = Nothing
    , optSyncGroup = Nothing,   optForce = False
    , optUnsafe = False,        optFast = go False
    }

do_program :: IO() -> Handle -> IO()
do_program gogo _ = gogo

gOptions :: [OptDescr (Options -> IO Options)]
gOptions = [
    Option ['v'] ["version"] (NoArg showV) "Display Version",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    
    Option []    ["make"]    (NoArg mkSharingan) "Create .sharingan.yml template",
    Option []    ["config"]  (NoArg config) "Edit .sharingan.yml config file",
    Option []    ["defaults"](NoArg defaultsConfig) "Edit .sharinganDefaults.yml config file",
    
    Option ['l'] ["list"]    (OptArg list "STRING") "List repositories",
    Option ['c'] ["add-curr"](NoArg getAC) "Add current repository",
    Option ['a'] ["add"]     (ReqArg getA "STRING") "Add repository",
    Option ['d'] ["delete"]  (ReqArg getD "STRING") "Delete repository / repositories",
    Option [] ["delete-curr"](NoArg getDC) "Delete current repository",
    
    Option []    ["enable"]  (ReqArg (enable True) "STRING") "Enable repository / repositories",
    Option []    ["disable"] (ReqArg (enable False) "STRING") "Disable repository / repositories",
    
    Option ['j'] ["jobs"]    (ReqArg getJ "STRING") "Maximum parallel jobs",
    Option ['s'] ["sync"]    (ReqArg gets "STRING") "sync single repository",
    Option ['g'] ["group"]   (ReqArg getg "STRING") "sync some repository group",
    
    Option ['u'] ["unsafe"]  (NoArg runUnsafe) "do not process reset before sync",
    Option ['q'] ["quick"]   (NoArg fastReinstall) "quick sync, don't process .sharingan.yml files",
    Option ['f'] ["force"]   (NoArg forceReinstall) "force process .sharingan.yml files",
    
    Option ['i'] ["interactive"] (NoArg interactive) "trying guess what to do for each repository"
  ]
  
winOptions :: [OptDescr (Options -> IO Options)]
winOptions = [
    Option ['D'] ["depot"]   (NoArg getDepot) "Get Google depot tools with git and python"
  ]
  
nixOptions :: [OptDescr (Options -> IO Options)]
nixOptions = [
    Option ['G'] ["Gentoo"]  (NoArg genS) "Synchronize cvs portagee tree Gentoo x86"
  ]
  
options :: [OptDescr (Options -> IO Options)]
options = gOptions ++ if | os `elem` ["win32", "mingw32", "cygwin32"] -> winOptions
                         | otherwise -> nixOptions

genSync    ::   Maybe String -> IO()
genSync j  =    gentooSync "/home/gentoo-x86" j >> exitWith ExitSuccess

getDepot        ::   Options -> IO Options
mkSharingan     ::   Options -> IO Options
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
getJ            ::   String -> Options -> IO Options
gets            ::   String -> Options -> IO Options
getg            ::   String -> Options -> IO Options

genS opt            = return opt { optG = True }
interactive opt     = return opt { optInteractive = True }
getJ arg opt        = return opt { optJobs = Just arg }
gets arg opt        = return opt { optSync = Just arg }
getg arg opt        = return opt { optSyncGroup = Just arg }
forceReinstall opt  = return opt { optForce = True }
runUnsafe opt       = return opt { optUnsafe = True }
fastReinstall opt   = return opt { optFast  = go True }

list arg _ =
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
         rsdata <- yDecode ymlx :: IO [Repository]
         let rdd = case arg of
                    Just s  -> filter (\r -> isInfixOf s (location r)) rsdata
                    Nothing -> rsdata
             maxl = maximum $ map (\x -> length $ last $ splitOn "\\" $ location x) rdd
         forM_ rdd $ \repo ->
            let loc  = location repo
                name = last $ splitOn "\\" loc
                lnam = (maxl + 1) - (length name)
                adds = if lnam > 0 then replicate lnam ' '
                                   else ""
                sstr = " - " ++ name ++ adds ++ " : "
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

go :: Bool -> [String] -> Bool -> Bool -> Bool -> Maybe String -> Maybe String -> Maybe String -> IO()
go fast nonops unsafe intera force syn synGroup _ =
  withDefaultsConfig $ \defx ->
   withConfig $ \ymlx ->                           
    let ymlprocess = ifSo $ despair $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        dfdata <- yDecode defx :: IO Defaults
        forM_ rsdata $ \repo ->
            let loc  = location repo
                gr   = syncGroup repo
                sync = case syn of
                            Nothing -> if (length nonops) > 0 then Just $ nonops !! 0
                                                              else Nothing
                            Just _  -> syn
                isenabled = case (enabled repo) of
                                Just en -> en
                                Nothing -> True
            in when (case sync of
                            Just snc -> isInfixOf snc loc
                            Nothing  -> case synGroup of
                                            Just sg -> case gr of
                                                        Just gg -> isenabled && gg == sg
                                                        Nothing -> False
                                            Nothing -> isenabled
                                        )
                $ let up  = splitOn " " $ upstream repo
                      tsk = task repo
                      br  = branches repo
                      ps  = postRebuild repo
                      hs  = hash repo
                      cln = case (clean repo) of
                               Just cl -> cl
                               Nothing -> False
                      noq = case (quick dfdata) of
                                Just qc -> not qc
                                Nothing -> True
                      u b = do printf " - %s : %s\n" loc b
                               amaterasu tsk loc b up unsafe cln hs $ if (length up) > 1
                                                                       then up !! 1 `elem` br
                                                                       else False
                      eye (_, r) = when ((r || force) && (not fast) && noq)
                                    $ do let shx = loc </> ".sharingan.yml"
                                         doesFileExist shx >>= sharingan intera shx loc
                                         when (isJust ps) $ forM_ (fromJust ps) $ \psc ->
                                                                let pshx = psc </> ".sharingan.yml"
                                                                in doesFileExist pshx
                                                                    >>= sharingan intera pshx psc
                  in do forM_ (tails br)
                         $ \case x:[] -> u x >>= eye -- Tail
                                 x:xs -> u x >>= (\_ -> return ())
                                 []   -> return ()
                        putStrLn <| replicate 89 '_'

    in doesFileExist ymlx >>= ymlprocess
