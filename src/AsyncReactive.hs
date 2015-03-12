{-# LANGUAGE LambdaCase, UnicodeSyntax #-}

module AsyncReactive
  ( asyncReactive
  ) where

import System.IO
import Prelude.Unicode

import Control.Exception
import Control.Monad
import Control.Eternal
import Control.Concurrent
import Control.Concurrent.Async

data Progress
    = Progress { pr_inc  :: IO ()
               , pr_done :: IO ()
               }
mkProgress :: Handle → IO Progress
mkProgress h = reactiveObjectIO 0 $ \ _pid req act done →
  Progress { pr_inc = do hPutStr h "."
                         hFlush h
           , pr_done = do hPutStr h "\nDone\n"
                          hFlush h 
                          done }

doProcess :: Progress → Async () → IO ()
doProcess r prc = 
    poll prc >>= \case Nothing → pr_inc r >> threadDelay 10000 >> doProcess r prc
                       Just _e → case _e of
                                   Left ex → putStrLn $ "Caught exception: " ⧺ show ex
                                   Right _ → pr_done r

asyncReactive :: IO () → IO ()
asyncReactive foo = liftM2_ doProcess (mkProgress stdout)
                                     $ async foo
