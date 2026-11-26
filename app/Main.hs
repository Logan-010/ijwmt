module Main (main) where

import Control.Concurrent.Async (cancel, waitAny)
import Control.Exception (bracket, onException)
import Control.Monad (unless, void)
import Data.Maybe (catMaybes)
import Network.BitTorrent.Socket (newTcp, newUdp)
import qualified Network.BitTorrent.Transport.TCP as TCP
import qualified Network.BitTorrent.Transport.UTP as UTP
import Network.PeerPool (acceptTCPTask, acceptUTPTask, newPeerPool)
import Network.Socket (close, listen)
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
              pool <- newPeerPool

              bracket
                ( if AG.noTcp c
                    then pure Nothing
                    else do
                      socket <- newTcp (AG.tcpAddr c)
                      Just <$> (listen socket 128 >> TCP.newTcpSocket socket) `onException` close socket
                )
                (mapM_ (close . TCP.inner))
                $ \tcp ->
                  bracket
                    ( if AG.noUtp c
                        then pure Nothing
                        else do
                          socket <- newUdp (AG.utpAddr c)
                          Just <$> UTP.newUTPSocket socket `onException` close socket
                    )
                    (mapM_ UTP.closeUTPSocket)
                    $ \utp ->
                      bracket
                        (traverse (acceptTCPTask pool) tcp)
                        (mapM_ cancel)
                        $ \tcpTask ->
                          bracket
                            (traverse (\manager -> UTP.newUTP manager >>= acceptUTPTask pool) utp)
                            (mapM_ cancel)
                            $ \utpTask -> do
                              let tasks = catMaybes [tcpTask, utpTask]

                              -- TODO
                              
                              unless (null tasks) (void $ waitAny tasks)
