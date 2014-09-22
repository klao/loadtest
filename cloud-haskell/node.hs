{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (threadDelay)
import Control.Monad (forever, forM_, when)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.Transport (EndPointAddress(..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.Environment (getArgs)
import System.IO

import qualified Latency as L

--------------------------------------------------------------------------------
-- Server

server :: Process ()
server =
  do myPid <- getSelfPid
     register "server" myPid
     say "Server started"
     forever $ receiveWait
       [ match $ \(_::Int) -> return ()
       , match $ \(_::Maybe [(Integer,Double)]) -> return ()
       , matchIf (\(x :: Rational) -> x < 179 / 918723817) $ \_ -> return ()
       , match $ \(_::[[[[[[[[Bool]]]]]]]]) -> return ()
       , match replyBack
       , match $ \sender -> send sender myPid
       ]
  where
    replyBack :: (ProcessId, ByteString) -> Process ()
    replyBack (sender, _msg) = send sender ("YYY" :: ByteString)

--------------------------------------------------------------------------------
-- Client

client :: NodeId -> Process ()
client serverNid = do
  myPid <- getSelfPid
  nsendRemote serverNid "server" myPid
  serverPid <- expect

  -- Annoy the server:
  -- replicateM_ 500 $ send serverPid (998172387/117 :: Rational)

  liftIO $ hSetBuffering stdout NoBuffering
  measure <- L.new
  forM_ [1..1000::Int] $ \k -> do
    L.start measure
    send serverPid (myPid, "XX" :: ByteString)
    (_ :: ByteString) <- expect
    L.stop measure
    liftIO $ do putStr "."
                when (k `mod` 40 == 0) $ putStr "\n"
                -- threadDelay 20000
  L.print measure

--------------------------------------------------------------------------------

data Role = Client | Server

main :: IO ()
main = do
  args <- getArgs
  let (me, them, role) = case args of
        ["server"] -> ("5501", "5502", Server)
        ["client"] -> ("5502", "5501", Client)
        _ -> error $ "usage: ./node <server|client>"

  Right t <- createTransport "127.0.0.1" me defaultTCPParameters
  node <- newLocalNode t initRemoteTable

  let theirNid = NodeId $ EndPointAddress $ BS.pack $ "127.0.0.1:" ++ them ++ ":0"
  case role of
    Server -> runProcess node server
    Client -> runProcess node $ client theirNid

  -- This is needed to see what the client process logs right before
  -- exiting:
  threadDelay 100000
