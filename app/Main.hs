{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Concurrent.Async (cancel, waitAny)
import Control.Exception (bracket, onException)
import Control.Monad (unless, void, (>=>))
import Data.BitTorrent (Torrent (version), TorrentV1 (infoHash), TorrentV2 (infoHash))
import qualified Data.BitTorrent as BT
import Data.BitTorrent.Magnet (Topic (..), topic)
import qualified Data.BitTorrent.Magnet as MG
import Data.BitTorrent.Wire (generatePeerId)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Maybe (catMaybes, maybeToList)
import Network.BitTorrent.Socket (getPort, newTcp, newUdp)
import Network.BitTorrent.Tracker (Event (Started))
import qualified Network.BitTorrent.Tracker as TR
import qualified Network.BitTorrent.Transport.TCP as TCP
import qualified Network.BitTorrent.Transport.UTP as UTP
import Network.BitTorrent.Util (initManager)
import Network.PeerPool (acceptTCPTask, acceptUTPTask, newPeerPool, registerTorrent)
import Network.Socket (close)
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
              manager <- initManager
              peerId <- generatePeerId
              let listenAddr port =
                    (if ':' `elem` AG.interface c then "[" ++ AG.interface c ++ "]" else AG.interface c)
                      ++ ":"
                      ++ show (port :: Int)

              bracket
                ( if AG.noTcp c
                    then pure Nothing
                    else do
                      newTcp (listenAddr (AG.port c)) >>= \socket ->
                        Just <$> TCP.newTcpSocket socket `onException` close socket
                )
                (mapM_ (close . TCP.inner))
                $ \tcp ->
                  bracket
                    ( if AG.noUtp c
                        then pure Nothing
                        else do
                          -- Reuse TCP's assigned port when the requested port is zero.
                          port <- maybe (pure (AG.port c)) (fmap fromIntegral . getPort . TCP.inner) tcp
                          newUdp (listenAddr port) >>= \socket ->
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

                                port <- case (tcp, utp) of
                                  (Just t, _) -> getPort t.inner
                                  (_, Just t) -> UTP.localPort t
                                  _ -> fail "No listeners"

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

                                let magnetTrackers =
                                      [ let ifh = case tpc of
                                              V1 b -> b
                                              V2 b -> b
                                         in TR.new manager url ifh peerId port 0 0 0 Started True 50
                                        | mg <- AG.magnets c,
                                          url <- MG.trackerUrl mg,
                                          tpc <- MG.topic mg
                                        ]
                                let torrentTrackers =
                                      [ TR.new manager url ifh peerId port 0 0 0 Started True 50
                                      | to <- AG.files c,
                                        url <- List.concat $ maybeToList (BT.announce to) : BT.announceList to,
                                        ifh <- case BT.version to of
                                            BT.V1 t -> [t.infoHash]
                                            BT.V2 t -> [t.infoHash]
                                            BT.Hybrid f t -> [f.infoHash, t.infoHash]
                                      ]

                                unless (null tasks) (void $ waitAny tasks)
