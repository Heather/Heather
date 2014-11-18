{-# LANGUAGE MultiWayIf, LambdaCase, OverloadedStrings, Arrows, CPP #-}

import Despair

import Model
import Yaml
import Tools
import Config
import SharinganProcess

import Text.Printf

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
    { optVerbosity :: Bool
    , optJobs :: Int
    }
    deriving Show
    
data SyncOpts = SyncOpts
    { syncForce :: Bool
    , syncUnsafe :: Bool
    , syncQuick :: Bool
    , syncInteractive :: Bool
    , syncFilter :: [String]
    , syncGroups :: [String]
    }
    deriving Show

data Command
    = Sync SyncOpts
    | MakeSharingan
    | Config
    | DefaultsConf
    | List [String]
    | Add [String] | Delete [String]
    | Enable String | Disable String
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
           <> command "list"        (info (listParser)          (progDesc "List repositories"))
           <> command "add"         (info (addParser)           (progDesc "Add repository (current path w/o args)"))
           <> command "delete"      (info (deleteParser)        (progDesc "Delete repository (current path w/o args)"))
           <> command "enable"      (info (Enable <$> (argument str (metavar "TARGET...")))
                                                                (progDesc "Enable repository / repositories"))
           <> command "disable"     (info (Disable <$> (argument str (metavar "TARGET...")))
                                                                (progDesc "Disable repository / repositories"))
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
           <> command "depot"       (info (pure Depot)          (progDesc "Get Google depot tools with git and python"))
#else
           <> command "update"      (info (pure Gentoo)         (progDesc "Synchronize cvs portagee tree Gentoo x86")) 
#endif
            ) -< ()
  A _version >>> A helper -< Args opts cmds

commonOpts :: Parser CommonOpts
commonOpts = runA $ proc () -> do
    v <- asA (switch ( short 'v'    <> long "verbose"
                                    <> help "Set verbosity to LEVEL")) -< ()
    j <- asA ( option auto ( short 'j'  <> long "jobs"
                                        <> metavar "JOBS"
                                        <> help "Maximum parallel jobs"
                                        <> value 0 )) -< ()
    returnA -< CommonOpts v j

listParser :: Parser Command
listParser = List <$> many (argument str (metavar "TARGET..."))

addParser :: Parser Command
addParser = Add <$> many (argument str (metavar "TARGET..."))

deleteParser :: Parser Command
deleteParser = Delete <$> many (argument str (metavar "TARGET..."))

syncParser :: Parser Command
syncParser = runA $ proc () -> do
    syncO <- asA syncOpts -< ()
    returnA -< Sync syncO

syncOpts :: Parser SyncOpts
syncOpts = runA $ proc () -> do
    force   <- asA (switch (short 'f' <> long "force"))         -< ()
    unsafe  <- asA (switch (short 'u' <> long "unsafe"))        -< ()
    quick   <- asA (switch (short 'q' <> long "quick"))         -< ()
    intera  <- asA (switch (short 'i' <> long "interactive"))   -< ()
    filter  <- asA (many (argument str (metavar "TARGET...")))  -< ()
    groups  <- asA (many (argument str (metavar "GROUP...")))   -< ()
    returnA -< SyncOpts { syncForce  = force
                        , syncUnsafe = unsafe
                        , syncQuick  = quick
                        , syncFilter = filter
                        , syncGroups = groups
                        , syncInteractive = intera
                        }

run :: Args -> IO ()
run (Args _ MakeSharingan)  = mkSharingan
run (Args _ Config)         = config
run (Args _ DefaultsConf)   = defaultsConfig
run (Args _ (List   xs))    = list xs
run (Args _ (Add    xs))    = getAC xs
run (Args _ (Delete xs))    = getDC xs
run (Args _ (Enable  xs))    = (enable True) xs
run (Args _ (Disable xs))    = (enable False) xs
run (Args opts (Sync so))   = sync opts so
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
run (Args _ getDepot)       = depot_tools
#else
run (Args opts Gentoo)      = genSync opts
#endif

main :: IO ()
main = execParser opts >>= run
  where opts = info parser
          ( fullDesc <> progDesc ""
                     <> header "Uchiha Dojutsu Kekkei Genkai [Mirror Wheel Eye]" )

sync :: CommonOpts -> SyncOpts -> IO ()
sync o so = do user <- getAppUserDataDirectory "sharingan.lock"
               lock <- doesFileExist user
               let run = withFile user WriteMode (do_program (go o so))
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

#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
genSync :: CommonOpts -> IO()
genSync o = let jobs = CommonOpts optJobs
            in gentooSync "/home/gentoo-x86" jobs
                    >> exitWith ExitSuccess
#endif

list :: [String] -> IO()
list xs =
  withConfig $ \ymlx ->
    let ymlprocess = ifSo $ do
         rsdata <- yDecode ymlx :: IO [Repository]
         let rdd = case xs of [] -> rsdata
                              _  -> filter (\r -> isInfixOf (xs !! 0) (location r)) rsdata
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

go :: CommonOpts -> SyncOpts -> IO()
go o so = let force    = syncForce so
              fast     = syncQuick so
              unsafe   = syncUnsafe so
              filter   = syncFilter so
              groups   = syncGroups so
              intera   = syncInteractive so
  in withDefaultsConfig $ \defx ->
   withConfig $ \ymlx ->                           
    let ymlprocess = ifSo $ despair $ do
        rsdata <- yDecode ymlx :: IO [Repository]
        dfdata <- yDecode defx :: IO Defaults
        forM_ rsdata $ \repo ->
            let loc  = location repo
                gr   = syncGroup repo
                sync = case filter of -- TODO
                            [] -> Nothing
                            _ -> Just $ filter !! 0
                isenabled = case (enabled repo) of
                                Just en -> en
                                Nothing -> True
            in when (case sync of
                            Just snc -> isInfixOf snc loc
                            Nothing  -> case groups of
                                            [] -> isenabled
                                            _  -> case gr of Just gg -> isenabled && (gg `elem` groups)
                                                             Nothing -> False
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
