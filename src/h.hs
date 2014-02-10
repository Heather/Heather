{-# LANGUAGE CPP, MultiWayIf, OverloadedStrings #-}
{----------------------------------------------------------------------------------------}
import Text.Printf
import Text.Show

import System.Environment( getArgs )
import System.Info (os)
import System.Directory
import System.Process
import System.Exit
import System.Console.GetOpt
import System.IO

import System.Environment.Executable ( getExecutablePath )

import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Exception

import System.FilePath(takeDirectory, (</>))

import Data.Yaml
import Data.Maybe (fromJust)

import qualified Data.ByteString.Char8 as BS
{----------------------------------------------------------------------------------------}
main = do user <- getAppUserDataDirectory "h.lock"
          locked <- doesFileExist user
          let run = myThreadId >>= \t -> withFile user WriteMode (do_program t)
                                           `finally` removeFile user
          if locked then do
                        putStrLn "There is already one instance of this program running."
                        putStrLn "Remove lock and start application? (Y/N)"
                        hFlush stdout
                        str <- getLine
                        if | str `elem` ["Y", "y"] -> run
                           | otherwise             -> return ()
                      else run
{----------------------------------------------------------------------------------------}
data Options = Options  {
    optPlatform  :: String,
    optForce :: String -> IO()
  }
{----------------------------------------------------------------------------------------}
defaultOptions :: Options
defaultOptions = Options {
    optPlatform = if | os `elem` ["win32", "mingw32", "cygwin32"] -> "Win"
                     | os `elem` ["darwin"] -> "Mac"
                     | otherwise -> "Linux"
        ,
    optForce = go False
  }
{----------------------------------------------------------------------------------------}
do_program :: ThreadId -> Handle -> IO ()
do_program t h = let s = "Locked by thread: " ++ show t
                 in do  putStrLn s
                        hPutStr h s
                        args <- getArgs
                        let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
                        opts <- foldl (>>=) (return defaultOptions) actions
                        let Options { optPlatform   = platform,
                                      optForce      = run } = opts
                        run platform
{----------------------------------------------------------------------------------------}
options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showV) "Display Version",
    Option ['h'] ["help"]    (NoArg showHelp) "Display Help",
    Option ['p'] ["platform"](ReqArg getp "STRING") "operating system platform",
    Option ['f'] ["force"]   (NoArg forceReinstall) "force reinstall even if same version is installed"
  ]
showV _    =    printf "h 0.0.1" >> exitWith ExitSuccess
showHelp _ = do putStrLn $ usageInfo "Usage: h [optional things]" options
                exitWith ExitSuccess
{----------------------------------------------------------------------------------------}
getp arg opt        = return opt { optPlatform = arg }
forceReinstall opt  = return opt { optForce = go True }
{----------------------------------------------------------------------------------------}
lyricsBracket = bracket_
     ( do
            putStrLn " ________________________________________________________ "
            putStrLn "          And who the hell do you think I've become?      "
            putStrLn "  Like the person inside, I've been opening up.           "
            putStrLn "                            I'm onto you. (I'm onto you.) "
            putStrLn " ________________________________________________________ "
    )( do
            putStrLn " ________________________________________________________ "
            putStrLn " Cut out your tongue and feed it to the liars.            "
            putStrLn "     Black hearts shed light on dying words.              "
            putStrLn "                                                          "
            putStrLn "                                 I wanna feel you burn.   "
            putStrLn " ________________________________________________________ "
            putStrLn ""
    )
{----------------------------------------------------------------------------------------}
data Repository = Repository {location :: String,
                              branch :: String}
                              deriving (Show)
{----------------------------------------------------------------------------------------}
instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .: "location" <*>
                           v .: "branch"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Repository from YAML/JSON"
{----------------------------------------------------------------------------------------}
exec :: [Char] -> IO()
exec args = do
    pid <- runCommand args
    waitForProcess pid >> return ()
{----------------------------------------------------------------------------------------}
exc :: [Char] -> [Char] -> IO()
exc path args = exec $ "cd " ++ path ++ " & " ++ args
{----------------------------------------------------------------------------------------}
rebasefork :: [Char] -> [Char] -> IO()
rebasefork path branch = do
    doesDirectoryExist path >>= (flip when
        $ do exc path $ "git checkout " ++ branch
                ++ " & git rebase --abort & git pull origin " ++ branch
                ++ " & git fetch upstream master & git pull --rebase upstream master"
                ++ " & git push --force origin " ++ branch)
{----------------------------------------------------------------------------------------}
go :: Bool -> String -> IO()
go pl force = (</> "sync.yml")
 <$> takeDirectory 
 <$> getExecutablePath >>= \ymlx ->
    doesFileExist ymlx >>= (flip when $ lyricsBracket
        $ do ymlData <- BS.readFile ymlx
             let ymlDecode = Data.Yaml.decode ymlData :: Maybe [Repository]
                 repoData  = fromJust ymlDecode
             forM_ repoData $ \repo -> do
                print repo
                liftA2 rebasefork location branch repo)
{----------------------------------------------------------------------------------------}