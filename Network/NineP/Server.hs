-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Listening on sockets for the incoming requests.
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Network.NineP.Server
	( module Network.NineP.Internal.File
	, Config(..)
	, run9PServer
	) where

import Control.Concurrent
import Control.Concurrent.MState hiding (get, put)
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.EmbedIO
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.NineP
import Data.Word
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.IO
import System.Log.Logger
import Text.Regex.Posix ((=~))

import Network.NineP.Error
import Network.NineP.Internal.File
import Network.NineP.Internal.Msg
import Network.NineP.Internal.State

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

listenOn addr = do
	sock <- socket AF_UNIX Stream defaultProtocol
	bind sock addr
	listen sock 5
	return sock

connection :: String -> IO Socket
connection s = let	pat = "tcp!(.*)!([0-9]*)|unix!(.*)" :: ByteString
			wrongAddr = ioError $ userError $ "wrong 9p connection address: " ++ s
			(bef, _, aft, grps) = s =~ pat :: (String, String, String, [String])
	in if (bef /= "" || aft /= "" || grps == [])
		then wrongAddr
		else case grps of
			[addr, port, ""] -> listen' addr $ toEnum $ (fromMaybe 2358 $ maybeRead port :: Int)
			["", "", addr]  -> listenOn $ SockAddrUnix addr
			_ -> wrongAddr

listen' :: HostName -> PortNumber -> IO Socket
listen' hostname port = do
	proto <- getProtocolNumber "tcp"
	bracketOnError (socket AF_INET Stream proto) close (\sock -> do
		setSocketOption sock ReuseAddr 1
		he <- getHostByName hostname
		bind sock (SockAddrInet port (hostAddress he))
		listen sock maxListenQueue
		return sock)

-- |Run the actual server using the supplied configuration.
run9PServer :: (EmbedIO m) => Config m -> IO ()
run9PServer cfg = do
	s <- connection $ addr cfg
	serve s cfg

serve :: (EmbedIO m) => Socket -> Config m -> IO ()
serve s cfg = forever $ accept s >>= (
		\(s, _) -> (doClient cfg) =<< (liftIO $ socketToHandle s ReadWriteMode))

doClient :: (EmbedIO m) => Config m -> Handle -> IO ()
doClient cfg h = do
	hSetBuffering h NoBuffering
	chan <- (newChan :: IO (Chan Msg))
	st <- forkIO $ sender (readChan chan) (BS.hPut h . BS.concat . B.toChunks) -- make a strict bytestring
	receiver cfg h (writeChan chan)
	killThread st
	hClose h

recvPacket :: Handle -> IO Msg
recvPacket h = do
	-- TODO error reporting
	s <- B.hGet h 4
	let l = fromIntegral $ runGet getWord32le $ assert (B.length s == 4) s
	p <- B.hGet h $ l - 4
	let m = runGet (get :: Get Msg) (B.append s p)
	debugM "Network.NineP.Server" $ show m
	return m

sender :: IO Msg -> (ByteString -> IO ()) -> IO ()
sender get say = forever $ do
	msg <- get
	debugM "Network.NineP.Server" $ show msg
	say $ runPut $ put msg

receiver :: (EmbedIO m) => Config m -> Handle -> (Msg -> IO ()) -> IO ()
receiver cfg h say = runReaderT (runMState (iterateUntil id (do
			mp <- liftIO $ try $ recvPacket h
			case mp of
				Left (e :: SomeException) -> do
					pure $ errorM "Network.NineP.Server" $ show e
					return True
				Right p -> do
					forkM $ handleMsg say p
					return False
		) >> return ()
	) (emptyState $ monadState cfg)) cfg >> return ()

handleMsg :: (EmbedIO m) => (Msg -> IO ()) -> Msg -> MState (NineState m) (ReaderT (Config m) IO) ()
handleMsg say p = do
	let Msg typ t m = p
	r <- try (case typ of
			TTversion -> rversion p
			TTattach -> rattach p
			TTwalk -> rwalk p
			TTstat -> rstat p
			TTwstat -> rwstat p
			TTclunk -> rclunk p
			TTauth -> rauth p
			TTopen -> ropen p
			TTread -> rread p
			TTwrite -> rwrite p
			TTremove -> rremove p
			TTcreate -> rcreate p
			TTflush -> rflush p
		)
	case r of
		(Right response) -> liftIO $ mapM_ say $ response
		-- FIXME which exceptions should i catch?
		(Left fail) -> liftIO $ say $ Msg TRerror t $ Rerror $ show $ (fail :: SomeException)
