{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import System.ZMQ4.Monadic

main :: IO ()
main = runZMQ $ do
  sock <- socket Rep
  setIpv6 True sock
  bind sock "tcp://*:5500"

  forever $ do
    _msg <- receive sock
    send sock [] "YYY"
