{-# LANGUAGE Arrows            #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           Config
import           Despair
import           EnvChecker
import           Sync                       (list, status, synchronize)
import           Tools

import           Text.Printf

import           System.Directory
import           System.Exit
import           System.FilePath            ((</>))
import           System.Info                (os)
import           System.IO

import           Data.Char                  (toLower)
import           Data.List
import           Data.Monoid
import           Data.List.Split
import           Data.Maybe
import           Data.Version               (showVersion)

import           Control.Arrow.Unicode
import           Control.Exception
import           Options.Applicative
import           Options.Applicative.Arrows

#if __GLASGOW_HASKELL__ <= 702
import           Data.Monoid
(<>) ∷ Monoid α ⇒ α → α → α
(<>) = mappend
#endif

_version ∷ Parser (α → α) -- ( づ ◔‿◔ )づ
_version = infoOption ("Sharingan " ⧺ showVersion version ⧺ " " ⧺ os)
  ( long "version" <> help "Print version information" )

parser ∷ Parser Args -- ✌(★◇★ )
parser = runA $ proc () → do
  opts ← asA commonOpts ⤙ ()
  cmds ← asA $ hsubparser
      ( command "sync"        (info syncParser            (progDesc "Process synchronization"))
     <> command "make"        (info (pure MakeSharingan)  (progDesc "Create .sharingan.yml template"))
     <> command "config"      (info (pure Config)         (progDesc "Edit Sharingan Network config"))
     <> command "defaults"    (info (pure DefaultsConf)   (progDesc "Edit Sharingan Defaults config"))
     <> command "list"        (info listParser            (progDesc "List Sharingan Network"))
     <> command "status"      (info statusParser          (progDesc "Display Sharingan build status for Network"))
     <> command "add"         (info addParser             (progDesc "Add repository to Sharingan Network (current path w/o args)"))
     <> command "delete"      (info deleteParser          (progDesc "Delete repository from Sharingan Network (current path w/o args)"))
     <> command "enable"      (info (Enable <$> argument str (metavar "TARGET..."))
                                                          (progDesc "Enable repository / repositories"))
     <> command "disable"     (info (Disable <$> argument str (metavar "TARGET..."))
                                                          (progDesc "Disable repository / repositories"))
      ) <|> syncParser ⤙ () -- ( ◜ ◉﹏◉)◜⌐■-■
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
run (Args _ MakeSharingan) = mkSharingan
run (Args _ Config)        = config
run (Args _ DefaultsConf)  = defaultsConfig
run (Args _ (List   xs))   = list xs
run (Args _ (Status xs))   = status xs
run (Args _ (Add    as))   = addNew as
run (Args _ (Delete xs))   = getDC xs
run (Args _ (Enable  xs))  = enable True xs
run (Args _ (Disable xs))  = enable False xs
run (Args opts (Sync σ))   = sync opts σ

main ∷ IO ()
main = execParser opts ≫= run
  where opts = info parser
          ( fullDesc <> progDesc ""
                     <> header "Uchiha Dojutsu Kekkei Genkai [Mirror Wheel Eye]" )

addNew ∷ AddOpts → IO ()
addNew α = getAC ( sFilter α )
                 ( sType   α )
                 ( sGroup  α )

sync           -- check locking file and process synchronization
  ∷ CommonOpts -- common options
  → SyncOpts   -- synchronization options
  → IO ()
sync o σ = do user ← getAppUserDataDirectory "sharingan.lock"
              lock ← doesFileExist user
              let runWithBlock = withFile user WriteMode (do_program (synchronize o σ))
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
