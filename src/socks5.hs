import           Data.Attoparsec            as A
import           Data.ByteString            as B (ByteString, pack)

import           Data.Monoid
import           Data.Word

import           Network.Socket

import           Control.Concurrent.Async

import           Control.Proxy              as P
import           Control.Proxy.Attoparsec   as P
import           Control.Proxy.Parse        as P
import           Control.Proxy.TCP          as P
import           Control.Proxy.Trans.Either as P


sockVer :: A.Parser Word8
sockVer = word8 5

reserved :: A.Parser Word8
reserved = word8 0

{--
        +----+----------+----------+
        |VER | NMETHODS | METHODS  |
        +----+----------+----------+
        | 1  |    1     | 1 to 255 |
        +----+----------+----------+

--}
authMsg :: A.Parser Word8
authMsg = do
    sockVer
    num <- anyWord8
    choice . replicate (fromIntegral num) $ word8 0
    return 0

{--
        +----+--------+
        |VER | METHOD |
        +----+--------+
        | 1  |   1    |
        +----+--------+
--}

authResp :: B.ByteString
authResp = B.pack [5, 0]

{--
        +----+-----+-------+------+----------+----------+
        |VER | CMD |  RSV  | ATYP | DST.ADDR | DST.PORT |
        +----+-----+-------+------+----------+----------+
        | 1  |  1  | X'00' |  1   | Variable |    2     |
        +----+-----+-------+------+----------+----------+
--}
type HandshakeTokens = (Word8, B.ByteString, B.ByteString)

-- todo: replace HandshakeTokens with SockAddr
data SockAddr = SockAddr4 HostAddress PortNumber
              | SockAddrFQDN B.ByteString PortNumber
              | SockAddr6 HostAddress6 PortNumber

cmdMsg :: A.Parser (Word8, HandshakeTokens)
cmdMsg = do
    sockVer
    cmd <- satisfy $ \w -> w == 1 -- || w == 2 || w == 3
    reserved
    atyp <- satisfy $ \w -> w == 1 || w == 3 || w == 4
    addrToken <- case atyp of
        1 -> A.take 4
        4 -> A.take 16
        3 -> do
            maybeLen  <- peekWord8
            case maybeLen of
                Just len -> A.take (fromIntegral len + 1)
                Nothing -> fail "ugh"

    portToken <- A.take 2
    return (cmd, (atyp, addrToken, portToken))

{--
        +----+-----+-------+------+----------+----------+
        |VER | REP |  RSV  | ATYP | BND.ADDR | BND.PORT |
        +----+-----+-------+------+----------+----------+
        | 1  |  1  | X'00' |  1   | Variable |    2     |
        +----+-----+-------+------+----------+----------+
--}

cmdResp :: HandshakeTokens -> B.ByteString
cmdResp (atyp, addrToken, portToken) =
    B.pack [5, 0, 0, atyp] <> addrToken <> portToken

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
handshake
  :: (Proxy p, Monad m) =>
  () -> P.Pipe
            (P.EitherP P.ParsingError (P.StateP [B.ByteString] p))
            (Maybe B.ByteString)
            B.ByteString
            m HandshakeTokens
handshake () = do
    _ <- P.parse authMsg
    P.respond authResp
    (_, dst) <- P.parse cmdMsg
    P.respond $ cmdResp dst
    return dst

main :: IO ()
main = serve (Host "127.0.0.1") "8000" $ \(cs, _) -> do
    (x, leftovers) <- P.runProxy $ P.runStateK mempty $ P.runEitherK $
        wrap . socketReadS 4096 cs >-> handshake >-> socketWriteD cs
    case x of
        Left _ -> error "whatever"
        Right (atyp, addrToken, portToken) ->
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

