module Socks5.Internal where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString       as B (ByteString, length, pack)
import           Data.ByteString.Char8 as B (unpack)
import           Data.List             as L (length)

import           Text.Printf

import           Control.Exception
import qualified Network.Socket        as NS

{--
        +----+----------+----------+
        |VER | NMETHODS | METHODS  |
        +----+----------+----------+
        | 1  |    1     | 1 to 255 |
        +----+----------+----------+
--}

newtype AuthMSG = AuthMSG (Word8, [Word8])

instance Binary AuthMSG where
    get = do
        _   <- sockVer
        num <- getWord8
        if num == 0
            then fail "failed"
            else do
                met <-replicateM (fromIntegral num) get
                let b = pick met
                case b of
                    Nothing -> fail "failed"
                    Just m -> return (AuthMSG (m, met))
        where
            pick [] = Nothing
            pick ms = case 0 `elem` ms of
                True -> Just 0
                False -> Nothing


    put (AuthMSG (_, met)) = do
        putWord8 5
        let num = fromIntegral $ L.length met
        putWord8 num
        forM_ met putWord8


{--
        +----+--------+
        |VER | METHOD |
        +----+--------+
        | 1  |   1    |
        +----+--------+
--}


newtype AuthResp = AuthResp Word8

authError :: AuthResp
authError = AuthResp 255

authSuccess :: AuthResp
authSuccess = AuthResp 0

instance Binary AuthResp where
    get = do
        _    <- sockVer
        resp <- get
        return $ AuthResp resp

    put (AuthResp resp) = do
        putWord8 5
        putWord8 resp

{--
        +----+-----+-------+------+----------+----------+
        |VER | CMD |  RSV  | ATYP | DST.ADDR | DST.PORT |
        +----+-----+-------+------+----------+----------+
        | 1  |  1  | X'00' |  1   | Variable |    2     |
        +----+-----+-------+------+----------+----------+
--}

newtype CmdMSG = CmdMSG (Word8, Socks5Addr)

instance Binary CmdMSG where
    get = do
        _    <- sockVer
        cmd  <- get
        _    <- reserved
        addr <- get
        return $ CmdMSG (cmd, addr)

    put (CmdMSG (cmd, addr)) = do
        putWord8 5
        putWord8 cmd
        putWord8 0
        put addr

{--
        +----+-----+-------+------+----------+----------+
        |VER | REP |  RSV  | ATYP | BND.ADDR | BND.PORT |
        +----+-----+-------+------+----------+----------+
        | 1  |  1  | X'00' |  1   | Variable |    2     |
        +----+-----+-------+------+----------+----------+
--}

newtype CmdResp = CmdResp (Word8, Maybe Socks5Addr)

connError :: CmdResp
connError = CmdResp (1, Nothing)

connSuccess :: Socks5Addr -> CmdResp
connSuccess addr = CmdResp (0, Just addr)

instance Binary CmdResp where
    get = do
        _    <- sockVer
        cmd  <- get
        _    <- reserved
        addr <- get
        return $ CmdResp (cmd, Just addr)

    put (CmdResp (cmd, addr)) = do
        putWord8 5
        putWord8 cmd
        putWord8 0
        case addr of
            Nothing ->
                putByteString $ B.pack [1, 0, 0, 0, 0, 0, 0]
            Just addr' ->
                put addr'

data Socks5Addr = Socks5Addr4 NS.HostAddress NS.PortNumber
                | Socks5AddrFQDN ByteString NS.PortNumber
                | Socks5Addr6 NS.HostAddress6 NS.PortNumber

toString :: Socks5Addr -> (String, String)
toString (Socks5Addr4 h p) = (showIPv4 h, show p)
toString (Socks5Addr6 h p) = (showIPv6 h, show p)
toString (Socks5AddrFQDN h p) = (B.unpack h, show p)

instance Binary Socks5Addr where
    get = do
        atyp <- getWord8
        case atyp of
            1 -> do
                h <- getWord32be
                p <- getWord16host
                return $ Socks5Addr4 h (NS.PortNum p)
            4 -> do
                w1 <- getWord32be
                w2 <- getWord32be
                w3 <- getWord32be
                w4 <- getWord32be
                p  <- getWord16host
                return $ Socks5Addr6 (w1, w2, w3, w4) (NS.PortNum p)
            3 -> do
                len  <- getWord8
                addr <- getByteString $ fromIntegral len
                p    <- getWord16host
                return $ Socks5AddrFQDN addr (NS.PortNum p)
            _ -> fail "Invalid Address type"

    put (Socks5Addr4 w (NS.PortNum p))                = do
        putWord8 1
        putWord32be w
        putWord16be p

    put (Socks5Addr6 (w1, w2, w3, w4) (NS.PortNum p)) = do
        putWord8 4
        putWord32be w1
        putWord32be w2
        putWord32be w3
        putWord32be w4
        putWord16be p

    put (Socks5AddrFQDN addr (NS.PortNum p))          = do
        putWord8 3
        putWord8 $ fromIntegral $ B.length addr
        putByteString addr
        putWord16be p

word8 :: Word8 -> Get Word8
word8 word = do
    res <- get
    if word == res
        then return res
        else fail "match failed"

sockVer :: Get Word8
sockVer = word8 5

reserved :: Get Word8
reserved = word8 0


-- Credit:
--  showIPv4 and showIPv6 are from 'iproute' package
--  it's available at http://hackage.haskell.org/package/iproute
--  under BSD3 license
showIPv4 :: NS.HostAddress -> String
showIPv4 = show4
  where
    remQuo x = (x `mod` 256, x `div` 256)
    show4 q = printf "%d.%d.%d.%d" a1 a2 a3 a4
      where
        (a4,q4) = remQuo q
        (a3,q3) = remQuo q4
        (a2,q2) = remQuo q3
        (a1, _) = remQuo q2

showIPv6 :: NS.HostAddress6 -> String
showIPv6 (a1,a2,a3,a4) = show6 a1 ++ ":" ++ show6 a2 ++ ":" ++ show6 a3 ++ ":" ++ show6 a4
  where
    remQuo x = (x `mod` 65536, x `div` 65536)
    show6 q = printf "%02x:%02x" r1 r2
      where
        (r2,q2) = remQuo q
        (r1, _) = remQuo q2

-- Credit:
--   'connect' is mostly copied from "network-simple".
--   the only difference is I added the "notify" part.

connect host port notify = bracketNotify connectSock notify (NS.sClose . fst)
    where
        connectSock = do
            (addr:_) <- NS.getAddrInfo (Just hints) (Just host) (Just port)
            bracketOnErrorNotify
                (newSocket addr)
                notify
                NS.sClose
                $ \sock -> do
                    let sockAddr = NS.addrAddress addr
                    NS.connect sock sockAddr
                    return (sock, sockAddr)

newSocket :: NS.AddrInfo -> IO NS.Socket
newSocket addr = NS.socket (NS.addrFamily addr)
                           (NS.addrSocketType addr)
                           (NS.addrProtocol addr)

bracketNotify before notifier after thing =
  mask $ \restore -> do
    a <- restore before `onException` notifier
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

bracketOnErrorNotify before notifier after thing =
  mask $ \restore -> do
    a <- restore before `onException` notifier
    restore (thing a) `onException` after a

hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG]
                        , NS.addrSocketType = NS.Stream }


