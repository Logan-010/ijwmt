module Network.BitTorrent.Socket (newTcp, newUdp, getPort) where

import qualified Data.List.NonEmpty as NE
import Network.Socket

newSocket :: HostName -> ServiceName -> SocketType -> IO Socket
newSocket address port socketType = do
  let hints = defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICHOST], addrSocketType = socketType}
  addr <- NE.head <$> getAddrInfo (Just hints) (Just address) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  Network.Socket.bind sock (addrAddress addr)
  pure sock

parseAddr :: String -> Maybe (String, String)
parseAddr addr = case addr of
  '[' : rest ->
    case break (== ']') rest of
      (host, ']' : ':' : port) -> Just (host, port)
      _ -> Nothing
  _ ->
    case break (== ':') addr of
      (host, ':' : port) -> Just (host, port)
      _ -> Nothing

getPort :: Socket -> IO PortNumber
getPort sock = socketPort sock

newTcp :: String -> IO Socket
newTcp addr =
  case parseAddr addr of
    Just (a, p) -> newSocket a p Stream
    Nothing -> ioError $ userError "Invalid address"

newUdp :: String -> IO Socket
newUdp addr =
  case parseAddr addr of
    Just (a, p) -> newSocket a p Datagram
    Nothing -> ioError $ userError "Invalid address"
