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
import Sync

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
parser = Args
  <$> commonOpts
  <*> (hsubparser
      ( command "sync"        (info syncParser
                              (progDesc "Process synchronization"))
     <> command "make"        (info (pure MakeSharingan)
                              (progDesc "Create .sharingan.yml template"))
     <> command "config"      (info (pure Config)
                              (progDesc "Edit .sharingan.yml config file"))
     <> command "defaults"    (info (pure DefaultsConf)
                              (progDesc "Edit .sharinganDefaults.yml config file"))
     <> command "list"        (info listParser
                              (progDesc "List repositories"))
     <> command "status"      (info statusParser
                              (progDesc "Sharingan build statuses for repositories"))
     <> command "add"         (info addParser
                              (progDesc "Add repository (current path w/o args)"))
     <> command "delete"      (info deleteParser
                              (progDesc "Delete repository (current path w/o args)"))
     <> command "enable"      (info (Enable <$> argument str (metavar "TARGET..."))
                              (progDesc "Enable repository / repositories"))
     <> command "disable"     (info (Disable <$> argument str (metavar "TARGET..."))
                              (progDesc "Disable repository / repositories"))
#if ( defined(mingw32_HOST_OS) || defined(__MINGW32__) )
     <> command "depot"       (info (pure Depot)
                              (progDesc "Get / Update Google depot tools with git and python"))
#endif
  ) <|> syncParser)

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

sync           -- check locking file and process synchronization
  ∷ CommonOpts -- common options
  → SyncOpts   -- synchronization options
  → IO ()
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

mkSharingan ∷ IO ()
mkSharingan = -- Create .sharingan.yml template
  let langM = Just "haskell"
      envM  = Just []
      biM   = Just []
      iM    = Just []
      new   = SharinganWrapper (Sharingan langM envM biM iM ["cabal install"])
  in yEncode ".sharingan.yml" new ≫ exitSuccess
