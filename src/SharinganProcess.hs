module SharinganProcess
  ( sharingan,
    gentooSync,
    rebasefork
  ) where

import Yaml
import Shell

import Data.Char (toLower)

import Control.Monad
import Control.Eternal

sharingan :: String -> String -> Bool -> IO()
sharingan shx loc shxi = if shxi then
     do syncDatax <- yDecode shx :: IO Sharingan
        let lang = map toLower $ language syncDatax
            en = env syncDatax
            be = before_install syncDatax
            il = install syncDatax
            sc = script syncDatax
        forM_ en $ setEnv
        forM_ be $ exc loc
        case il of
          [] -> case lang of
                  "haskell" -> exc loc "cabal update"
                  _         -> return () -- do nothing
          _ -> forM_ il $ exc loc
        case sc of
          [] -> case lang of
                  "c"       -> exc loc "make"
                  "haskell" -> exc loc "cabal install"
                  "rust"    -> exc loc "make"
                  _         -> return () -- do nothing
          _ -> forM_ sc $ exc loc
     else 
      do doesFileExist "make" >>= ifSo $ exc loc "make"
        
