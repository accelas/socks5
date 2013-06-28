import           Data.ByteString            as B (ByteString, pack)
import           Data.Binary.Get                 (Get)

import           Data.Monoid
import           Data.Word

import           Network.Socket

import           Control.Concurrent.Async

import           Control.Proxy              as P
import           Control.Proxy.Binary       as P
import           Control.Proxy.Parse        as P
import           Control.Proxy.TCP          as P
import           Control.Proxy.Trans.Either as P

{--
        +----+----------+----------+
        |VER | NMETHODS | METHODS  |
        +----+----------+----------+
        | 1  |    1     | 1 to 255 |
        +----+----------+----------+

        +----+--------+
        |VER | METHOD |
        +----+--------+
        | 1  |   1    |
        +----+--------+

        +----+-----+-------+------+----------+----------+
        |VER | CMD |  RSV  | ATYP | DST.ADDR | DST.PORT |
        +----+-----+-------+------+----------+----------+
        | 1  |  1  | X'00' |  1   | Variable |    2     |
        +----+-----+-------+------+----------+----------+

        +----+-----+-------+------+----------+----------+
        |VER | REP |  RSV  | ATYP | BND.ADDR | BND.PORT |
        +----+-----+-------+------+----------+----------+
        | 1  |  1  | X'00' |  1   | Variable |    2     |
        +----+-----+-------+------+----------+----------+
--}

data Socks5Addr = Socks5Addr4 HostAddress PortNumber
              | Socks5AddrFQDN B.ByteString PortNumber
              | Socks5Addr6 HostAddress6 PortNumber

instance Socks5Addr where
    get = do
        atyp <- getWord8
        dst  <- case atyp of
            1 -> getAddr4
            4 -> getAddr6
            3 -> getAddrFqdn
            _ -> fail "does not compute"
        where
            getAddr4 = Socks5Addr4 <$> getWord32be <*> getWord16be
            getAddr6 = do
                w1 <- getWord32be
                w2 <- getWord32be
                w3 <- getWord32be
                w4 <- getWord32be
                p  <- getWord16be
                return Socks5Addr6 (w1, w2, w3, w4) p
            getAddrFqdn = do
                len  <- getWord8
                addr <- getByteString len
                p    <- getWord16be
                return Socks5AddrFQDN addr p

    put = undefined

data AuthMSG = Methods [Word8]

word8 :: Word8 -> Get Word8
word8 word = do
    res <- get
    if (word == res)
        then return res
        else fail "match failed"

sockVer :: Get Word8
sockVer = word8 5

reserved :: Get Word8
reserved = word8 0

authMsg :: Get AuthMSG
authMsg = do
    _   <- sockVer
    num <- get
    return $ sequence $ replicate (fromIntegral num) get


authResp :: B.ByteString
authResp = B.pack [5, 0]


cmdMsg :: Get (Word8, Socks5Addr)
cmdMsg = do
    _   <- sockVer
    cmd <- get
    _   <- reserved
    dst <- get
    return (cmd, dst)

cmdResp :: Socks5Addr -> B.ByteString
cmdResp dst =
    B.pack [5, 0, 0] <> encode dst

-----------------------------------------------------------------------------
handshake
  :: (Proxy p, Monad m) =>
  () -> P.Pipe
            (P.EitherP P.ParsingError (P.StateP [B.ByteString] p))
            (Maybe B.ByteString)
            B.ByteString
            m Socks5Addr
handshake () = do
    methods <- P.decode authMsg
    case pick methods of
        Nothing -> fail "fail"
        just _  -> do
            P.respond authResp
            (_, dst) <- P.decode cmdMsg
            -- P.respond $ cmdResp dst
            return dst

main :: IO ()
main = serve (Host "127.0.0.1") "8000" $ \(cs, _) -> do
    (x, leftovers) <- P.runProxy $ P.runStateK mempty $ P.runEitherK $
        wrap . socketReadS 4096 cs >-> handshake >-> socketWriteD cs
    case x of
        Left _ -> -- todo send failed response
        Right dst ->

            P.connect addr port $ \(ss, _ ) -> do
                s1 <- async (runProxy $ combined              >-> P.socketWriteD ss)
                s2 <- async (runProxy $ P.socketReadS 4096 ss >-> P.socketWriteD cs)
                waitEither_ s1 s2
            where
                addr = undefined
                port = undefined
                -- Leftovers come before further reads from the `cs` socket
                combined () = do
                    mapM_ respond leftovers
                    P.socketReadS 4096 cs ()

