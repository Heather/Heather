{-# LANGUAGE MultiWayIf, LambdaCase, OverloadedStrings, Arrows, CPP #-}

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

import Paths_Sharingan (version)
import Data.Version (showVersion)

import Options.Applicative
import Options.Applicative.Arrows
#if __GLASGOW_HASKELL__ <= 702
import Data.Monoid
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Args = Args CommonOpts Command deriving Show
  
data CommonOpts = CommonOpts
    { optVerbosity :: Int 
    }
    deriving Show
  
data Command
    = Sync
    | MakeSharingan
    | Config
    | DefaultsConf
    | List
    | Depot
    | Gentoo
    deriving Show

_version :: Parser (a -> a)
_version = infoOption ("Sharingan " ++ (showVersion version) ++ " " ++ os)
  (  long "version" <> help "Print version information" )

parser :: Parser Args
parser = runA $ proc () -> do
  opts <- asA commonOpts -< ()
  cmds <- (asA . hsubparser)
            ( command "sync"        (info syncParser            (progDesc "Process synchronization"))
           <> command "make"        (info (pure MakeSharingan)  (progDesc "Create .sharingan.yml template"))
           <> command "config"      (info (pure Config)         (progDesc "Edit .sharingan.yml config file"))
           <> command "defaults"    (info (pure DefaultsConf)   (progDesc "Edit .sharinganDefaults.yml config file"))
           <> command "list"        (info (pure List)           (progDesc "List repositories"))
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
           <> command "depot"       (info (pure Depot)          (progDesc "Get Google depot tools with git and python"))
#else
           <> command "update"      (info (pure Gentoo)         (progDesc "Synchronize cvs portagee tree Gentoo x86")) 
#endif
            ) -< ()
  A _version >>> A helper -< Args opts cmds

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
  <$> option auto
      ( short 'v'
         <> long "verbose"
         <> metavar "LEVEL"
         <> help "Set verbosity to LEVEL"
         <> value 0 )

syncParser :: Parser Command
syncParser = pure Sync

run :: Args -> IO ()
run (Args _ MakeSharingan)  = mkSharingan
run (Args _ Config)         = config
run (Args _ DefaultsConf)   = defaultsConfig
run (Args _ List)           = list Nothing      -- TODO: args
run (Args opts Sync)        = sync opts         -- TODO: args
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
run (Args _ getDepot)       = depot_tools
#else
run (Args opts Gentoo)      = genSync Nothing   -- TODO: args
#endif

main :: IO ()
main = execParser opts >>= run
  where opts = info parser
          ( fullDesc <> progDesc ""
                     <> header "Uchiha Dojutsu Kekkei Genkai [Mirror Wheel Eye]" )

sync :: CommonOpts -> IO ()
sync o = do user <- getAppUserDataDirectory "sharingan.lock"
            lock <- doesFileExist user
            let gogo = go False [] False False False Nothing Nothing
                run = withFile user WriteMode (do_program gogo)
                         `finally` removeFile user
            if lock then do putStrLn "There is already one instance of this program running."
                            putStrLn "Remove lock and start application? (Y/N)"
                            hFlush stdout
                            str <- getLine
                            if str `elem` ["Y", "y"] then run
                                                     else return ()
                    else run
  where do_program :: IO() -> Handle -> IO()
        do_program gogo _ = gogo

{- TODO: add options
gOptions :: [OptDescr (Options -> IO Options)]
gOptions = [
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
-}

#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
genSync    ::   Maybe String -> IO()
genSync j  =    gentooSync "/home/gentoo-x86" j >> exitWith ExitSuccess
#endif

list :: Maybe String -> IO()
list arg =
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

mkSharingan :: IO ()
mkSharingan = -- Create .sharingan.yml template
  let langM = Just "haskell"
      envM  = Just []
      biM   = Just []
      iM    = Just []
      new   = (Sharingan langM envM biM iM ["cabal install"])
  in yEncode ".sharingan.yml" new >> exitWith ExitSuccess

go :: Bool -> [String] -> Bool -> Bool -> Bool -> Maybe String -> Maybe String -> IO()
go fast nonops unsafe intera force syn synGroup =
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
