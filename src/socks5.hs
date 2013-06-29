import           Data.ByteString            as B (ByteString)
import           Data.ByteString.Lazy       as BL (toStrict)

import           Control.Concurrent.Async
import           Data.Binary                as Bin (Binary, encode)
import           Data.Monoid
import           Debug.Trace

import           Control.Proxy              as P
import qualified Control.Proxy.Binary       as P
import qualified Control.Proxy.Parse        as P (wrap)
import           Control.Proxy.TCP          as P
import           Control.Proxy.Trans.Either as P (EitherP, runEitherK)
import           Control.Proxy.Trans.State  as P (StateP, evalStateK)

import           Socks5.Internal            as I

-----------------------------------------------------------------------------
handshake :: (Proxy p, Monad m) =>
    () -> P.Pipe
        (P.EitherP P.DecodingError (P.StateP [B.ByteString] p))
        (Maybe B.ByteString)
        B.ByteString m Socks5Addr
handshake () =  do
    AuthMSG (_, _) <- P.decode
    P.respond $ toStrictByteString authSuccess
    CmdMSG (_, dst) <- P.decode
    -- todo: send response After connection success
    P.respond $ toStrictByteString $ connSuccess dst
    return dst

toStrictByteString :: Bin.Binary a => a -> B.ByteString
toStrictByteString = BL.toStrict . Bin.encode

main :: IO ()
main = P.serve (P.Host "127.0.0.1") "8000" $ \(cs, caddr) -> trace ("request from " ++ show caddr ) $ do
    let sendMsg msg = runProxy $
            -- why does this line need 4 more space of indentation?!
            (\() -> P.respond $ toStrictByteString msg) >-> P.socketWriteD cs

    res <- P.runProxy $ P.evalStateK mempty $ P.runEitherK $
        P.wrap . P.socketReadS 4096 cs >-> handshake >-> P.socketWriteD cs

    case res of

        -- todo, if decoding cmd msg fails, it should be sending authError
        Left _ -> sendMsg authError
        Right dst -> trace ("connecting " ++ host ++ ":" ++ port) $
            P.connect host port $ \(ss, _ ) -> trace ("connetion established") $ do
                s1 <- async (runProxy $ P.socketReadS 4096 cs >-> P.socketWriteD ss)
                s2 <- async (runProxy $ P.socketReadS 4096 ss >-> P.socketWriteD cs)
                waitEither_ s1 s2
            where
                (host, port) = toString dst
                notify = sendMsg connError

