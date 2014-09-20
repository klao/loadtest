import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
-- import Control.Distributed.Process.Internal.Types
import Control.Distributed.Process.Node
import qualified Data.ByteString.Char8 as BS
import Network.Transport (EndPointAddress(..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.Environment (getArgs)

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, _msg) = send sender "YYY"

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

server :: Process ()
server = do
  myPid <- getSelfPid
  register "server" myPid
  say "Server started"
  -- ProcessId _ myPid <- getSelfPid
  -- liftIO $ print myPid
  forever $ receiveWait [match logMessage, match replyBack]

client :: NodeId -> Process ()
client serverNid = do
  myPid <- getSelfPid
  nsendRemote serverNid "server" "hello"
  nsendRemote serverNid "server" (myPid, "XX")

  m <- expectTimeout 1000000
  case m of
    Nothing  -> die "nothing came back!"
    Just s   -> say $ "got " ++ s ++ " back!"


main :: IO ()
main = do
  [role] <- getArgs
  let (me, them) = case role of
        "server" -> ("5501", "5502")
        "client" -> ("5502", "5501")
        _ -> error $ "bad role: " ++ role

  Right t <- createTransport "127.0.0.1" me defaultTCPParameters
  node <- newLocalNode t initRemoteTable

  let theirNid = NodeId $ EndPointAddress $ BS.pack $ "127.0.0.1:" ++ them ++ ":0"
  -- print theirNid
  case role of
    "server" -> runProcess node server
    "client" -> runProcess node $ client theirNid
    _ -> error "bad role"

  threadDelay 100000
