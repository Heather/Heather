{-# LANGUAGE MultiWayIf, LambdaCase, OverloadedStrings, Arrows, CPP #-}

import Despair
import Tools
import Config
import SharinganProcess
import Paths_Sharingan (version)

import Text.Printf

import System.Info (os)
import System.Directory
import System.Exit
import System.IO
import System.FilePath(takeDirectory, (</>))

import Data.List.Split
import Data.Version (showVersion)

import Options.Applicative
import Options.Applicative.Arrows

#if __GLASGOW_HASKELL__ <= 702
import Data.Monoid
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

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
	   <> command "depot"       (info (pure Depot)          (progDesc "Get / Update Google depot tools with git and python"))
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
    filter  <- asA (optional (argument str (metavar "FILTER"))) -< ()
    groups  <- asA (many (option str (short 'g'  <> long "group" <> metavar "GROUPS"))) -< ()
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
run (Args _ (Enable  xs))   = (enable True) xs
run (Args _ (Disable xs))   = (enable False) xs
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
               let runWithBlock = withFile user WriteMode (do_program (synchronize o so))
									`finally` removeFile user
               if lock then do putStrLn "There is already one instance of this program running."
                               putStrLn "Remove lock and start application? (Y/N)"
                               hFlush stdout
                               str <- getLine
                               when (str `elem` ["Y", "y"]) runWithBlock
                       else runWithBlock
  where do_program :: IO() -> Handle -> IO()
        do_program gogo _ = gogo

#if ! ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
genSync :: CommonOpts -> IO()
genSync o = gentooSync "/home/gentoo-x86" (optJobs o)
                 >> exitWith ExitSuccess
#endif

list :: [String] -> IO()
list xs = withConfig $ \ymlx -> do
    rsdata <- yDecode ymlx :: IO [Repository]
    let rdd = case xs of [] -> rsdata
                         _  -> filter (\r -> isInfixOf (xs !! 0) (location r)) rsdata
        maxl = maximum $ map (\x -> length $ last $ splitOn "\\" $ location x) rdd
    forM_ rdd $ \repo ->
       let loc  = location repo
           name = last $ splitOn "\\" loc
           lnam = (maxl + 1) - (length name)
           sstr = " - " ++ name ++ if lnam > 0 then replicate lnam ' '
                                               else ""
           empt = replicate (length sstr) ' '
           brx  = branches repo
       in if (length brx) == 0
           then printf " - %s\n" loc
           else do printf "%s: %s (%s)\n" sstr (head brx) loc
                   forM_ (drop 1 brx) $ printf "%s: %s\n" empt
    exitWith ExitSuccess

mkSharingan :: IO ()
mkSharingan = -- Create .sharingan.yml template
  let langM = Just "haskell"
      envM  = Just []
      biM   = Just []
      iM    = Just []
      new   = (Sharingan langM envM biM iM ["cabal install"])
  in yEncode ".sharingan.yml" new >> exitWith ExitSuccess

synchronize :: CommonOpts -> SyncOpts -> IO()
synchronize o so =
  withDefaultsConfig $ \defx ->
   withConfig $ \ymlx -> despair $ do
    rsdata <- yDecode ymlx :: IO [Repository]
    dfdata <- yDecode defx :: IO Defaults
    forM_ rsdata $ \repo ->
        let loc = location repo
            isenabled = fromMaybe True (enabled repo)
        in when (case syncFilter so of
                        Nothing  -> case syncGroups so of
                                        [] -> isenabled
                                        gx  -> case syncGroup repo of 
                                                    Just gg -> isenabled && (gg `elem` gx)
                                                    Nothing -> False
                        Just snc -> isInfixOf snc loc)
            $ let ups = splitOn " " $ upstream repo
                  cln = fromMaybe False (clean repo)
                  noq = not $ fromMaybe False (quick dfdata)
                  u b = do printf " - %s : %s\n" loc b
                           amaterasu (task repo) loc b ups (syncUnsafe so) cln (hash repo) 
                                            $ if (length ups) > 1 then ups !! 1 `elem` (branches repo)
                                                                  else False
                  eye (_, r) = when ((r || syncForce so) && (not $ syncQuick so) && noq)
                                $ do let shx = loc </> ".sharingan.yml"
                                         ps  = postRebuild repo
                                     doesFileExist shx >>= sharingan (syncInteractive so) shx loc
                                     when (isJust ps) $ forM_ (fromJust ps) $ \psc ->
                                                            let pshx = psc </> ".sharingan.yml"
                                                            in doesFileExist pshx
                                                                >>= sharingan (syncInteractive so) pshx psc
              in do forM_ (tails (branches repo))
                     $ \case x:[] -> u x >>= eye -- Tail
                             x:xs -> u x >>= (\_ -> return ())
                             []   -> return ()
                    putStrLn <| replicate 89 '_'
