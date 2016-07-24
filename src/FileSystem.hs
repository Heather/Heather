{-# LANGUAGE Safe          #-}
{-# LANGUAGE UnicodeSyntax #-}

module FileSystem
  ( copyDir
  ) where

import           Control.Monad          (forM_)
import           System.Directory
  ( copyFile
  , createDirectory
  , doesDirectoryExist
  , getDirectoryContents
  )

import           System.FilePath        ((</>))

import           Control.Eternal.Syntax

copyDir ∷ FilePath -- source
         → FilePath -- destination
         → IO ()
copyDir src dst = do
  createDirectory dst
  content ← getDirectoryContents src
  let xs = filter (∉ [".", ".."]) content
  forM_ xs $ \name → let srcPath = src </> name
                         dstPath = dst </> name
      in doesDirectoryExist srcPath >>= \dirExist →
          if dirExist then copyDir srcPath dstPath
                      else copyFile srcPath dstPath
