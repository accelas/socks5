module Socks5.Internal where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString as B (ByteString, length, pack)
import           Data.List       as L (length)
import           Network.Socket

{--
        +----+----------+----------+
        |VER | NMETHODS | METHODS  |
        +----+----------+----------+
        | 1  |    1     | 1 to 255 |
        +----+----------+----------+
--}

newtype AuthMSG = AuthMSG (Word8, [Word8])

authError :: AuthResp
authError = AuthResp 255

authSuccess :: AuthResp
authSuccess = AuthResp 0

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
        forM_ met $ putWord8


{--
        +----+--------+
        |VER | METHOD |
        +----+--------+
        | 1  |   1    |
        +----+--------+
--}

newtype AuthResp = AuthResp Word8

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

connError :: CmdResp
connError = CmdResp (1, Nothing)

connSuccess :: Socks5Addr -> CmdResp
connSuccess addr = CmdResp (0, Just addr)

newtype CmdResp = CmdResp (Word8, Maybe Socks5Addr)

instance Binary CmdResp where
    get = do
        _    <- sockVer
        cmd  <- get
        addr <- get
        return $ CmdResp (cmd, (Just addr))

    put (CmdResp (cmd, addr)) = do
        putWord8 5
        putWord8 cmd
        putWord8 0
        case addr of
            Nothing -> do
                putByteString $ B.pack [1, 0, 0, 0, 0, 0, 0]
            Just addr' ->
                put addr'

data Socks5Addr = Socks5Addr4 HostAddress PortNumber
                | Socks5AddrFQDN ByteString PortNumber
                | Socks5Addr6 HostAddress6 PortNumber

instance Binary Socks5Addr where
    get = do
        atyp <- getWord8
        case atyp of
            1 -> do
                h <- getWord32be
                p <- getWord16be
                return $ Socks5Addr4 h (PortNum p)
            4 -> do
                w1 <- getWord32be
                w2 <- getWord32be
                w3 <- getWord32be
                w4 <- getWord32be
                p  <- getWord16be
                return $ Socks5Addr6 (w1, w2, w3, w4) (PortNum p)
            3 -> do
                len  <- getWord8
                addr <- getByteString $ fromIntegral len
                p    <- getWord16be
                return $ Socks5AddrFQDN addr (PortNum p)
            _ -> fail "Invalid Address type"

    put (Socks5Addr4 w (PortNum p))                = do
        putWord32be w
        putWord16be p

    put (Socks5Addr6 (w1, w2, w3, w4) (PortNum p)) = do
        putWord32be w1
        putWord32be w2
        putWord32be w3
        putWord32be w4
        putWord16be p

    put (Socks5AddrFQDN addr (PortNum p))          = do
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

