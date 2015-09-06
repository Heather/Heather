{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
  , Arrows
  , CPP
  , UnicodeSyntax
  #-}

import Despair
import EnvChecker
import Tools
import Config
import SharinganProcess

import Text.Printf

import System.Info (os)
import System.Directory
import System.Exit
import System.IO
import System.FilePath ((</>))

import Data.Char (toLower)
import Data.List
import Data.Maybe
import Data.List.Split
import Data.Version (showVersion)

import Options.Applicative
import Options.Applicative.Arrows
import Control.Arrow.Unicode
import Control.Exception

#if __GLASGOW_HASKELL__ <= 702
import Data.Monoid
(<>) ∷ Monoid a ⇒ a → a → a
(<>) = mappend
#endif

_version ∷ Parser (a → a) -- ( づ ◔‿◔ )づ
_version = infoOption ("Sharingan " ⧺ showVersion version ⧺ " " ⧺ os)
  ( long "version" <> help "Print version information" )

parser ∷ Parser Args -- ✌(★◇★ )
parser = runA $ proc () → do
  opts ← asA commonOpts ⤙ ()
  cmds ← (asA . hsubparser)
      ( command "sync"        (info syncParser            (progDesc "Process synchronization"))
     <> command "make"        (info (pure MakeSharingan)  (progDesc "Create .sharingan.yml template"))
     <> command "config"      (info (pure Config)         (progDesc "Edit .sharingan.yml config file"))
     <> command "defaults"    (info (pure DefaultsConf)   (progDesc "Edit .sharinganDefaults.yml config file"))
     <> command "list"        (info listParser            (progDesc "List repositories"))
     <> command "status"      (info statusParser          (progDesc "Sharingan build statuses for repositories"))
     <> command "add"         (info addParser             (progDesc "Add repository (current path w/o args)"))
     <> command "delete"      (info deleteParser          (progDesc "Delete repository (current path w/o args)"))
     <> command "enable"      (info (Enable <$> argument str (metavar "TARGET..."))
                                                          (progDesc "Enable repository / repositories"))
     <> command "disable"     (info (Disable <$> argument str (metavar "TARGET..."))
                                                          (progDesc "Disable repository / repositories"))
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
     <> command "depot"       (info (pure Depot)          (progDesc "Get / Update Google depot tools with git and python"))
     <> command "cabal"       (info (pure Cabal)          (progDesc "Cabal upgrade"))
#endif
      ) ⤙ () -- ( ◜ ◉﹏◉)◜⌐■-■
  A _version ⋙ A helper ⤙ Args opts cmds

commonOpts ∷ Parser CommonOpts
commonOpts = runA $ proc () → do
    v ← asA (switch ( short 'v' <> long "verbose"
                                <> help "Set verbosity to LEVEL")) ⤙ ()
    j ← asA ( option auto ( short 'j'  <> long "jobs"
                                       <> metavar "JOBS"
                                       <> help "Maximum parallel jobs"
                                       <> value 0 )) ⤙ ()
    returnA ⤙ CommonOpts v j

listParser ∷ Parser Command
listParser = List <$> many (argument str (metavar "TARGET..."))

statusParser ∷ Parser Command
statusParser = Status <$> many (argument str (metavar "TARGET..."))

addParser ∷ Parser Command
addParser = runA $ proc () → do
    addO ← asA addOpts ⤙ ()
    returnA ⤙ Add addO

addOpts ∷ Parser AddOpts
addOpts = runA $ proc () → do -- ԅ(O‿O ԅ )
  _stype   ← asA (optional (option str (short 't'  <> long "type" <> metavar "TYPE"))) ⤙ ()
  _sgroup  ← asA (optional (option str (short 'g'  <> long "group" <> metavar "GROUP"))) ⤙ ()
  _sfilter ← asA (optional (argument str (metavar "FILTER"))) ⤙ ()
  returnA ⤙ AddOpts { sType   = _stype
                    , sFilter = _sfilter
                    , sGroup  = _sgroup
                    }

deleteParser ∷ Parser Command
deleteParser = Delete <$> many (argument str (metavar "TARGET..."))

syncParser ∷ Parser Command
syncParser = runA $ proc () → do
    syncO ← asA syncOpts ⤙ ()
    returnA ⤙ Sync syncO

syncOpts ∷ Parser SyncOpts
syncOpts = runA $ proc () → do -- ԅ(O‿O ԅ )
  full   ← asA (switch (long "full"))                       ⤙ ()
  force  ← asA (switch (short 'f' <> long "force"))         ⤙ ()
  unsafe ← asA (switch (short 'u' <> long "unsafe"))        ⤙ ()
  quick  ← asA (switch (short 'q' <> long "quick"))         ⤙ ()
  intera ← asA (switch (short 'i' <> long "interactive"))   ⤙ ()
  nopush ← asA (switch (long "no-push"))                    ⤙ ()
  filter ← asA (optional (argument str (metavar "FILTER"))) ⤙ ()
  groups ← asA (many (option str (short 'g'  <> long "group" <> metavar "GROUPS"))) ⤙ ()
  returnA ⤙ SyncOpts { syncFull = full
                      , syncForce  = force
                      , syncUnsafe = unsafe
                      , syncQuick  = quick
                      , syncFilter = filter
                      , syncGroups = groups
                      , syncInteractive = intera
                      , syncNoPush = nopush
                      }

run ∷ Args → IO () -- (＠ ・‿‿・)
run (Args _ MakeSharingan)  = mkSharingan
run (Args _ Config)         = config
run (Args _ DefaultsConf)   = defaultsConfig
run (Args _ (List   xs))    = list xs
run (Args _ (Status xs))    = status xs
run (Args _ (Add    as))    = addNew as
run (Args _ (Delete xs))    = getDC xs
run (Args _ (Enable  xs))   = enable True xs
run (Args _ (Disable xs))   = enable False xs
run (Args opts (Sync so))   = sync opts so
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
run (Args _ Depot)          = depotTools
run (Args _ Cabal)          = cabalUpgrade
#endif

main ∷ IO ()
main = execParser opts ≫= run
  where opts = info parser
          ( fullDesc <> progDesc ""
                     <> header "Uchiha Dojutsu Kekkei Genkai [Mirror Wheel Eye]" )

addNew ∷ AddOpts → IO ()
addNew ao = getAC ( sFilter ao )
                  ( sType   ao )
                  ( sGroup  ao )

sync ∷ CommonOpts → SyncOpts → IO ()
sync o so = do user ← getAppUserDataDirectory "sharingan.lock"
               lock ← doesFileExist user
               let runWithBlock = withFile user WriteMode (do_program (synchronize o so))
                                        `finally` removeFile user
               if lock then do putStrLn "There is already one instance of this program running."
                               putStrLn "Remove lock and start application? (Y/N)"
                               hFlush stdout
                               answer ← getLine
                               when (answer ∈ ["Y", "y"]) runWithBlock
                       else runWithBlock
  where do_program ∷ IO() → Handle → IO()
        do_program gogo _ = gogo

list ∷ [String] → IO() -- (＾‿‿＾ *)
list xs = withConfig $ \ymlx → do
  jsdat ← yDecode ymlx ∷ IO [RepositoryWrapper]
  let rsdata = map _getRepository jsdat
      rdd = case xs of [] → rsdata
                       _  → filter (isInfixOf (head xs) . location) rsdata
      maxl = maximum $ map (length . last . splitOn "\\" . location) rdd
  forM_ rdd $ \repo →
     let locs = location repo
         name = last $ splitOn "\\" locs
         lnam = maxl + 1 - length name
         sstr = " - " ⧺ name ⧺ if lnam > 0 then replicate lnam ' '
                                           else ""
         empt = replicate (length sstr) ' '
         brx  = branches repo
     in if length brx ≡ 0
         then printf " - %s\n" locs
         else do printf "%s: %s (%s) %s" sstr (head brx) locs (task repo)
                 unless (fromMaybe True (enabled repo))
                   $ putStr " [Disabled]"
                 putStrLn ""
                 forM_ (drop 1 brx) $ printf "%s: %s\n" empt

status ∷ [String] → IO() -- (＾‿‿＾ *)
status xs = withConfig $ \ymlx → do
  jsdata ← yDecode ymlx ∷ IO [RepositoryWrapper]
  let rsdata = map _getRepository jsdata
      rdd = case xs of [] → rsdata
                       _  → filter (isInfixOf (head xs) . location) rsdata
      maxl = maximum $ map (length . last . splitOn "\\" . location) rdd
  forM_ rdd $ \repo →
     let loc  = location repo
         name = last $ splitOn "\\" loc
         lnam = maxl + 1 - length name
         sstr = " - " ⧺ name ⧺ if lnam > 0 then replicate lnam ' '
                                           else ""
         stat = case positive repo of
                      Just True  → "OK" ∷ String
                      Just False → "Errors..."
                      Nothing    → "?"
     in printf "%s- %s\n" sstr stat

mkSharingan ∷ IO ()
mkSharingan = -- Create .sharingan.yml template
  let langM = Just "haskell"
      envM  = Just []
      biM   = Just []
      iM    = Just []
      new   = SharinganWrapper (Sharingan langM envM biM iM ["cabal install"])
  in yEncode ".sharingan.yml" new ≫ exitSuccess

synchronize ∷ CommonOpts → SyncOpts → IO()
synchronize _o so = -- ( ◜ ①‿‿① )◜
  withDefaultsConfig $ \defx →
   withConfig $ \ymlx → despair $ do
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
    when (syncFull so) cabalUpgrade
#endif
    jsdat ← yDecode ymlx ∷ IO [RepositoryWrapper]
    jfdat ← yDecode defx ∷ IO DefaultsWrapper
    myenv ← getEnv
    let dfdat = _getDefaults jfdat
        rsdat = map _getRepository jsdat
    forM_ rsdat $ sync myenv dfdat where
  sync myEnv dfdata repo =
    let loc = location repo
        isenabled = fromMaybe True (enabled repo)
    in when (case syncFilter so of
                    Nothing  → case syncGroups so of
                                    [] → isenabled
                                    gx  → case syncGroup repo of
                                                Just gg → isenabled ∧ (gg ∈ gx)
                                                Nothing → False
                    Just snc → isInfixOf <| map toLower snc
                                         <| map toLower loc)
      $ let ups = splitOn " " $ upstream repo
            cln = fromMaybe False (clean repo)
            adm = fromMaybe False (root repo)
            noq = not $ fromMaybe False (quick dfdata)
            snc = sharingan (syncInteractive so) adm
            frs = syncForce so
            ntr = syncInteractive so
            nps = syncNoPush so
            u b = do printf " - %s : %s\n" loc b
                     amaterasu (task repo) loc b ups (syncUnsafe so)
                               frs cln adm (hash repo) nps myEnv
            eye (_, r) = when ((r ∨ frs) ∧ not (syncQuick so) ∧ noq)
              $ do let shx = loc </> ".sharingan.yml"
                       ps  = postRebuild repo
                   doesFileExist shx ≫= sharingan
                        ntr adm shx loc
                   when (isJust ps) $ forM_ (fromJust ps) $ \psc →
                      let pshx = psc </> ".sharingan.yml"
                      in doesFileExist pshx≫= snc pshx psc
        in do forM_ (tails (branches repo))
               $ \case [x] → u x ≫= eye -- Tail
                       x:_ → u x ≫= (\_ → return ())
                       []  → return ()
              putStrLn ⊲ replicate 89 '_'
