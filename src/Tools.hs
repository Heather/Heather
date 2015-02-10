module Tools
  ( depot_tools
  ) where

import Codec.Archive.Zip

import System.Directory
import System.Process

import Control.Monad
import Control.Eternal

import qualified Data.ByteString.Lazy as B

depot_tools :: IO()
depot_tools =
    let src = "depot_tools"
        dst = "C:/depot_tools"
    in doesDirectoryExist dst >>= \dstExist -> if dstExist
           then do exc dst "git pull"
                   exc dst "gclient"
           else doesDirectoryExist src >>= \srcExist -> unless srcExist $ do
                    let tarball = "depot_tools.zip"
                    doesFileExist tarball >>= \fileExist -> unless fileExist $ do
                        putStrLn " -> Getting Depot Tools" 
                        download "http://src.chromium.org/svn/trunk/tools/depot_tools.zip" tarball
                        dictZipFile <- B.readFile tarball
                        extractFilesFromArchive [OptVerbose] $ toArchive dictZipFile
                        srcExists <- doesDirectoryExist src
                        dstExists <- doesDirectoryExist dst
                        if or [not srcExists, dstExists] 
                            then putStrLn " -> Can not copy to C:"
                            else copyDir src dst >> removeDirectoryRecursive src
                                                 >> removeFile tarball
                    {-          Here depot_tools must be added to PATH             -}
                    putStrLn "======================================================"
                    putStrLn " -> NOW! Move your ass and add C:/depot_tools to PATH" 
                    putStrLn " -> Press any key when it will be done or already done"
                    putStrLn "======================================================"
                    getChar >> return ()
                    {- I know..................................................... -}
                    exc dst "gclient"
