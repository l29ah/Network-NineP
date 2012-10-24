{-# LANGUAGE OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -pgmP cpp #-}

module HighNineP where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map (Map)
import qualified Data.Map as M
import Data.NineP
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO

import Debug.Trace

data NineFile =
	RegularFile {
        	nineRead :: Word64 -> Word32 -> IO (B.ByteString),
        	nineWrite :: Word64 -> B.ByteString -> IO (Word32),
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO ()
	} | Directory {
		getFiles :: IO (Map String NineFile),
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO ()
	}

data NineClient = NineClient {
        fidMap :: Map Word32 NineFile
}

boringStat :: Stat
boringStat = Stat 0 0 (Qid 0 0 0) 0 0 0 0 "boring" "" "" ""

boringFile :: String -> NineFile
boringFile name = RegularFile
        (\_ c -> return $ B.take (fromIntegral c) "i am so very boring")
        (\_ _ -> return 0)
        (return ())
        (return $ boringStat {st_name = name})
        (const $ return ())

boringDir :: String -> [(String, NineFile)] -> NineFile
boringDir name contents = Directory
	(return $ M.fromList contents)
        (return ())
        (return $ boringStat {st_name = name})
        (const $ return ())

run9PServer :: IO ThreadId
run9PServer = do
	s <- socket AF_INET Stream defaultProtocol
	setSocketOption s ReuseAddr 1
	bindSocket s (SockAddrInet 4242 iNADDR_ANY)
	listen s 10
	forkIO $ serve s

serve :: Socket -> IO ()
serve s = forever $ accept s >>= (
		\(s, _) -> doClient =<< (liftIO $ socketToHandle s ReadWriteMode))

doClient :: Handle -> IO ()
doClient h = do
	putStrLn "yay, a client"
	hSetBuffering h NoBuffering
	chan <- (newChan :: IO (Chan Msg))
	st <- forkIO $ sender (readChan chan) (B.hPut h)
	receiver h (writeChan chan)
	putStrLn "bye!"
	killThread st
	hClose h

recvPacket :: Handle -> IO Msg
recvPacket h = do
	-- TODO error reporting
	putStrLn "got packet"
	s <- B.hGet h 4
	let l = fromIntegral $ runGet getWord32le $ assert (B.length s == 4) s
	p <- B.hGet h $ l - 4
	let m = runGet (get :: Get Msg) (B.append s p)
	print m
	return m

sender :: IO Msg -> (ByteString -> IO ()) -> IO ()
sender get say = forever $ do
	(say . runPut . put . join traceShow) =<< get

receiver :: Handle -> (Msg -> IO ()) -> IO ()
receiver h say = S.evalStateT (iterateUntil id (do
		p <- liftIO $ recvPacket h
		let Msg typ t m = p
		case typ of
		{-
			TTversion -> (liftIO . say . Msg TRversion t) =<< rversion (Rversion) m
			TTattach -> (liftIO . say . Msg TRattach t) =<< rattach (Rattach) m
			TTwalk -> (liftIO . say . Msg TRwalk t) =<< rwalk (Rwalk) m
			-}
#define MSG(x) TT##x -> (liftIO . say . Msg TR##x t) =<< r##x (R##x) m
			MSG(version)
			--MSG(attach)
			--MSG(walk)
--			MSG(stat)
			--MSG(clunk)
#undef MSG
		return False) >> return ()
	) (M.empty :: Map Word32 Word64)

--TODO version check
rversion c (Tversion s _) = return $ c s "9P2000"

{-
rattach :: (Qid -> VarMsg) -> VarMsg -> S.StateT (Map Word32 Word64) IO VarMsg
rattach c (Tattach fid _ _ _) = do
	q <- asks rootQIDP
	mf <- asks mkFile
	f <- lift $ mf q
	S.modify (M.insert fid q)
	return $ c $ Qid (typ f) 0 q
-}
