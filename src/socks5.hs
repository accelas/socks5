import           Data.ByteString            as B (ByteString)
import           Data.ByteString.Lazy       as BL (toStrict)

import           Data.Monoid
import           Data.Binary as Bin (encode, Binary)

import           Network.Socket (Socket)

import           Control.Concurrent.Async

import           Control.Proxy              as P
import qualified Control.Proxy.Binary       as P
import qualified Control.Proxy.Parse        as P (wrap)
import           Control.Proxy.TCP          as P
import           Control.Proxy.Trans.Either as P (EitherP, runEitherK)
import           Control.Proxy.Trans.State  as P (StateP, evalStateK)

import           Socks5.Internal

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
    return dst

toStrictByteString :: Bin.Binary a => a -> B.ByteString
toStrictByteString = \x -> BL.toStrict $ Bin.encode x

main :: IO ()
main = serve (Host "127.0.0.1") "8000" $ \(cs, _) -> do
    let sendMsg msg = runProxy $ do
            -- why does this line need 4 more space of indentation?!
            (\() -> P.respond $ toStrictByteString msg) >-> P.socketWriteD cs

    res <- P.runProxy $ P.evalStateK mempty $ P.runEitherK $
        P.wrap . socketReadS 4096 cs >-> handshake >-> socketWriteD cs
    case res of
        Left _ -> sendMsg authError
        Right dst ->

            P.connect addr port $ \(ss, _ ) -> do
                sendMsg $ connSuccess dst
                s1 <- async (runProxy $ P.socketReadS 4096 cs >-> P.socketWriteD ss)
                s2 <- async (runProxy $ P.socketReadS 4096 ss >-> P.socketWriteD cs)
                waitEither_ s1 s2
            where
                addr = undefined
                port = undefined
                myconnect = undefined


{--
 -
 -

bracketNotify before notifier after thing =
  mask $ \restore -> do
    a <- restore (before) `onException` notifier
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

bracketOnErrorNotify before notifier after thing =
  mask $ \restore -> do
    a <- restore (before) `onException` notifier
    restore (thing a) `onException` after a

newSocket :: NS.AddrInfo -> IO NS.Socket
newSocket addr = NS.socket (NS.addrFamily addr)
                           (NS.addrSocketType addr)
                           (NS.addrProtocol addr)



connect host port = E.bracketNotify (connectSock host port) (NS.sClose . fst)
where
    connectSock host port = do
        (addr:_) <- NS.getAddrInfo (Just hints) (Just host) (Just port)
    bracketOnErrorNotify
        (newSocket addr)
        NS.sClose
        $ \sock -> do
       let sockAddr = NS.addrAddress addr
       NS.connect sock sockAddr
       return (sock, sockAddr)
  where
    hints = NS.defaultHints { NS.addrFlags = [NS.AI_ADDRCONFIG]
                            , NS.addrSocketType = NS.Stream }

 ---}
