module Network.BitTorrent.Util (initManager) where

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager, Manager)

initManager :: IO Manager
initManager = newManager tlsManagerSettings
