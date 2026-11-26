module Main (main) where

import Network.BitTorrent.Socket (newTcp, newUdp)
import qualified Network.BitTorrent.Transport.TCP as TCP
import qualified Network.BitTorrent.Transport.UTP as UTP
import qualified System.Args as AG
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  cli <- AG.parse args

  case cli of
    Left err -> hPutStrLn stderr $ "CLI parse error: " ++ err
    Right c ->
      if AG.help c
        then
          hPutStrLn stderr AG.helpStr
        else
          if AG.version c
            then
              hPutStrLn stderr AG.versionStr
            else do
              tcpSocket <-
                if AG.noTcp c
                  then
                    pure Nothing
                  else pure . TCP.newTcpSocket <$> newTcp (AG.tcpAddr c)
              utpSocket <-
                if AG.noUtp c
                  then
                    pure Nothing
                  else pure . UTP.newUTPSocket <$> newUdp (AG.utpAddr c)

              pure ()
