# IJWMT (I Just Want My Torrent)

<!--toc:start-->
- [IJWMT (I Just Want My Torrent)](#ijwmt-i-just-want-my-torrent)
  - [TO-DO](#to-do)
    - [Peer Transports](#peer-transports)
    - [Peer Discovery](#peer-discovery)
    - [Tracker Protocols](#tracker-protocols)
    - [BitTorrent Protocols and Extensions](#bittorrent-protocols-and-extensions)
<!--toc:end-->

Purely functional minimalist BitTorrent client in your terminal

## TO-DO

### Peer Transports

- [x] Unencrypted TCP
- [x] MSE/PE encryption
- [x] uTP over UDP (BEP 29)
- [ ] WebRTC DataChannels

### Peer Discovery

- [ ] Peer Exchange / ut_pex (BEP 11)
- [ ] Local Service Discovery (BEP 14)
- [ ] Mainline DHT (BEP 5)
- [ ] IPv6 DHT (BEP 32)
- [ ] DHT security extension (BEP 42)

### Tracker Protocols

- [x] HTTP trackers
- [x] HTTPS trackers
- [ ] UDP trackers (BEP 15)
- [x] Compact IPv4 and IPv6 peer lists
- [ ] Multi-tracker tiers (BEP 12)
- [ ] WS/WSS trackers

### BitTorrent Protocols and Extensions

- [x] BitTorrent v1 (BEP 3)
- [x] Extension protocol (BEP 10)
- [x] BitTorrent v2 and hybrid (BEP 52)
- [x] Fast extension (BEP 6)
- [ ] Magnet links / ut_metadata (BEP 9)
- [ ] Web seeds (BEP 19)
- [ ] NAT-PMP / PCP / UPnP IGD
- [ ] Hole punching (BEP 55)
