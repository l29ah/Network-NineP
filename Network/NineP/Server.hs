{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -pgmP cpp #-}

module Network.NineP.Server
	( module Network.NineP.Internal.File
	, Config(..)
	, run9PServer
	) where

import Control.Concurrent
--import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.Loops
--import qualified Control.Monad.State.Strict as S
import Control.Monad.RWS (RWST(..), evalRWST)
import Control.Monad.Reader.Class (asks)
import qualified Control.Monad.State.Class as S
import Control.Monad.Trans
import qualified Control.Monad.Writer.Class as W
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
import Network.Socket hiding (send, sendTo, recv, recvFrom)
--import Network.Socket.ByteString
import System.IO

import Network.NineP.Error
import Network.NineP.Internal.File
import Network.NineP.Internal.Msg
import Network.NineP.Internal.State

import Debug.Trace

run9PServer :: Config -> IO ()
run9PServer cfg = do
	s <- socket AF_INET Stream defaultProtocol
	setSocketOption s ReuseAddr 1
	bindSocket s (SockAddrInet 4242 iNADDR_ANY)
	listen s 10
	serve s cfg

serve :: Socket -> Config -> IO ()
serve s cfg = forever $ accept s >>= (
		\(s, _) -> (doClient cfg) =<< (liftIO $ socketToHandle s ReadWriteMode))

doClient :: Config -> Handle -> IO ()
doClient cfg h = do
	putStrLn "yay, a client"
	hSetBuffering h NoBuffering
	chan <- (newChan :: IO (Chan Msg))
	st <- forkIO $ sender (readChan chan) (BS.hPut h . BS.concat . B.toChunks) -- make a strict bytestring
	receiver cfg h (writeChan chan)
	putStrLn "bye!"
	killThread st
	hClose h

recvPacket :: Handle -> IO Msg
recvPacket h = do
	-- TODO error reporting
	s <- B.hGet h 4
	let l = fromIntegral $ runGet getWord32le $ assert (B.length s == 4) s
	p <- B.hGet h $ l - 4
	let m = runGet (get :: Get Msg) (B.append s p)
	print m
	return m

sender :: IO Msg -> (ByteString -> IO ()) -> IO ()
sender get say = forever $ do
	(say . runPut . put . join traceShow) =<< get

receiver :: Config -> Handle -> (Msg -> IO ()) -> IO ()
receiver cfg h say = evalRWST (iterateUntil id (do
			W.tell () -- satisfy the typechecker
			mp <- liftIO $ try $ recvPacket h
			case mp of
				Left (e :: SomeException) -> do
					return $ putStrLn $ show e
					return True
				Right p -> do
					handleMsg say p
					return False
		) >> return ()
	) cfg emptyState >> return ()

handleMsg say p = do
	let Msg typ t m = p
	r <- runErrorT (case typ of
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
		(Left fail) -> liftIO $ say $ Msg TRerror t $ Rerror $ show $ fail
