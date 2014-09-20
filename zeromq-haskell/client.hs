{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import System.IO
import System.ZMQ4.Monadic

import qualified Latency as L

main :: IO ()
main = runZMQ $ do
  sock <- socket Req
  setIpv6 True sock
  connect sock "tcp://[::1]:5500"
  liftIO $ threadDelay 100000

  liftIO $ hSetBuffering stdout NoBuffering
  measure <- L.new
  forM_ [1..1000::Int] $ \k -> do
    L.start measure
    send sock [] "XX"
    _msg <- receive sock
    L.stop measure
    liftIO $ do putStr "."
                when (k `mod` 40 == 0) $ putStr "\n"
                threadDelay 20000

  L.print measure
