{-# LANGUAGE LambdaCase
  , UnicodeSyntax
  , Safe
  #-}

module AsyncReactive
  ( asyncReactive
  ) where

import System.IO
import Prelude.Unicode

import Control.Eternal
import Control.Concurrent
import Control.Concurrent.Async

data Progress
    = Progress { prInc  :: IO ()
               , prDone :: IO ()
               }

mkProgress :: Handle → IO Progress
mkProgress h = reactiveObjectIO 0 $ \ _pid _ _ done →
  Progress { prInc = do hPutStr h "."
                        hFlush h
           , prDone = do hPutStr h "\nDone\n"
                         hFlush h
                         done }

doProcess :: Progress → Async () → IO ()
doProcess r prc =
  poll prc >>= \case Nothing → prInc r >> threadDelay 10000 >> doProcess r prc
                     Just _e → case _e of
                                 Left ex → putStrLn $ "Caught exception: " ⧺ show ex
                                 Right _ → prDone r

asyncReactive :: IO () → IO ()
asyncReactive foo = liftM2_ doProcess (mkProgress stdout)
                                     $ async foo
