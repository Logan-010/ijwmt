{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Concurrent.Async (cancel, waitAny)
import Control.Exception (bracket, onException)
import Control.Monad (unless, void, (>=>))
import Data.BitTorrent (Torrent (version), TorrentV1 (infoHash), TorrentV2 (infoHash))
import qualified Data.BitTorrent as BT
import Data.BitTorrent.Magnet (topic)
import qualified Data.BitTorrent.Magnet as MG
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Network.BitTorrent.Socket (newTcp, newUdp)
import qualified Network.BitTorrent.Transport.TCP as TCP
import qualified Network.BitTorrent.Transport.UTP as UTP
import Network.PeerPool (acceptTCPTask, acceptUTPTask, newPeerPool, registerTorrent)
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
                            (traverse (UTP.newUTP >=> acceptUTPTask pool) utp)
                            (mapM_ cancel)
                            $ \utpTask ->
                              do
                                let tasks = catMaybes [tcpTask, utpTask]

                                -- Create Callbacks
                                let mcb _ps = pure ()
                                    tcb _ps = pure ()
                                    mcb2 _ps = pure ()
                                    tcb2 _ps = pure ()

                                -- Register Magnets
                                mapM_
                                  ( mapM_
                                      ( \case
                                          MG.V1 b -> registerTorrent pool b mcb
                                          MG.V2 b -> registerTorrent pool (BS.take 20 b) mcb2
                                      )
                                      . topic
                                  )
                                  $ AG.magnets
                                    c

                                -- Register Torrents
                                mapM_
                                  ( \t -> case version t of
                                      BT.V1 v ->
                                        registerTorrent pool v.infoHash tcb
                                      BT.V2 v ->
                                        registerTorrent pool (BS.take 20 v.infoHash) tcb2
                                      BT.Hybrid f v ->
                                        registerTorrent pool f.infoHash tcb >> registerTorrent pool (BS.take 20 v.infoHash) tcb2
                                  )
                                  $ AG.files
                                    c

                                unless (null tasks) (void $ waitAny tasks)
