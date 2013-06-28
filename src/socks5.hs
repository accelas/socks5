import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString            as B (ByteString, length, pack)

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Data.Word

import           Network.Socket

import           Control.Concurrent.Async

import           Control.Proxy              as P
import qualified Control.Proxy.Binary       as P
import qualified Control.Proxy.Parse        as P
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
                | Socks5AddrFQDN ByteString PortNumber
                | Socks5Addr6 HostAddress6 PortNumber

instance Binary Socks5Addr where
    get = do
        atyp <- getWord8
        case atyp of
            1 -> Socks5Addr4 <$> getWord32be <*> getWord16be
            4 -> do
                w1 <- getWord32be
                w2 <- getWord32be
                w3 <- getWord32be
                w4 <- getWord32be
                p  <- getWord16be
                return Socks5Addr6 (w1, w2, w3, w4) p
            3 -> do
                len  <- getWord8
                addr <- getByteString len
                p    <- getWord16be
                return Socks5AddrFQDN addr p
            _ -> fail "does not compute"

    put (Socks5Addr4 w p)                = do
        putWord32be w
        putWord16be p

    put (Socks5Addr6 (w1, w2, w3, w4) p) = do
        putWord32be w1
        putWord32be w2
        putWord32be w3
        putWord32be w4
        putWord16be p

    put (Socks5AddrFQDN addr p)          = do
        putWord8 $ fromIntegral $ B.length addr
        putByteString addr
        putWord16be p

data AuthMSG = Methods [Word8]

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

authMsg :: Get AuthMSG
authMsg = do
    _   <- sockVer
    num <- get >>= liftM fromIntegral
    return $ replicateM num get

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
            (P.EitherP P.DecodingError (P.StateP [B.ByteString] p))
            (Maybe B.ByteString)
            B.ByteString
            m Socks5Addr
handshake () = do
    methods <- P.decode authMsg
    case pick methods of
        Nothing -> fail "fail"
        Just _  -> do
            P.respond authResp
            (_, dst) <- P.decode cmdMsg
            return dst
    where
        pick = undefined

main :: IO ()
main = serve (Host "127.0.0.1") "8000" $ \(cs, _) -> do
    x <- P.runProxy $ P.evalStateK mempty $ P.runEitherK $
        P.wrap . socketReadS 4096 cs >-> handshake >-> socketWriteD cs
    case x of
        Left _ -> undefined -- todo send failed response
            -- P.respond $ cmdResp dst
        Right dst ->
            P.connect addr port $ \(ss, _ ) -> do
                -- P.respond $ cmdResp dst
                s1 <- async (runProxy $ P.socketReadS 4096 cs >-> P.socketWriteD ss)
                s2 <- async (runProxy $ P.socketReadS 4096 ss >-> P.socketWriteD cs)
                waitEither_ s1 s2
            where
                addr = undefined
                port = undefined

