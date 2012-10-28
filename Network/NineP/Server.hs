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

import Debug.Trace

data Config = Config {
		root :: NineFile
	}

type Nine x = ErrorT NineError (RWST Config () (Map Word32 NineFile) IO) x

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
	) cfg (M.empty :: Map Word32 NineFile) >> return ()

handleMsg say p = do
	let Msg typ t m = p
	case typ of 
		TTflush -> return ()	-- not implemented
		_ -> do
			r <- runErrorT (case typ of
					TTversion -> rversion p
					TTattach -> rattach p
					TTwalk -> rwalk p
					TTstat -> rstat p
					TTclunk -> rclunk p
					TTauth -> rauth p
					TTopen -> ropen p
					TTread -> rread p
	 			)
			case r of
				(Right response) -> liftIO $ mapM_ say $ response
				(Left fail) -> liftIO $ say $ Msg TRerror t $ Rerror $ show $ fail

getQidTyp :: Stat -> Word8
getQidTyp s = fromIntegral $ shift (st_mode s) 24

makeQid :: NineFile -> Nine Qid
makeQid x = do
	s <- getStat x
	return $ Qid (getQidTyp s) 0 42

--TODO version check
rversion :: Msg -> Nine [Msg]
rversion (Msg _ t (Tversion s _)) = return $ return $ Msg TRversion t (Rversion s "9P2000")

rattach :: Msg -> Nine [Msg]
rattach (Msg _ t (Tattach fid _ _ _)) = do
	root <- asks root
	S.modify (M.insert fid root)
	q <- makeQid root
	return $ return $ Msg TRattach t $ Rattach q 

walk :: [Qid] -> [String] -> NineFile -> Nine (NineFile, [Qid])
walk qs [] f = return (f, qs)
walk qs (x:xs) (RegularFile {}) = throwError ENotADir
walk qs (x:xs) (Directory {descend = desc}) = do
	f <- mapErrorT lift $ desc x
	q <- makeQid f
	walk (q:qs) xs f

walk' = walk []

fidLookup :: Word32 -> Nine NineFile
fidLookup fid = do
	m <- S.get
	case M.lookup fid m of
		Nothing -> throwError $ ENoFid fid
		Just f -> return f

rwalk :: Msg -> Nine [Msg]
rwalk (Msg _ t (Twalk fid newfid path)) = do
	m <- S.get
	case M.lookup fid m of
		Nothing -> throwError $ ENoFid fid
		Just f -> do
			(nf, qs) <- walk' path f
			S.modify (M.insert newfid nf)
			return $ return $ Msg TRwalk t $ Rwalk $ qs

getStat :: NineFile -> Nine Stat
getStat f = do
	let fixDirBit = (case f of
				(RegularFile {}) -> flip clearBit 31
				(Directory {}) -> flip setBit 31
			)
	s <- lift $ lift $ stat f
	return s { st_mode = fixDirBit $ st_mode s,
		st_qid = (st_qid s) { qid_typ = getQidTyp s } }
	
rstat :: Msg -> Nine [Msg]
rstat (Msg _ t (Tstat fid)) = do
	m <- S.get
	let f = M.lookup fid m
	case f of
		Nothing -> throwError $ ENoFid fid
		Just (RegularFile _ _ _ _ _ _) -> do
			s <- getStat $ fromJust f
			return $ return $ Msg TRstat t $ Rstat $ [s]
		Just (Directory { getFiles = gF }) -> do
			contents <- lift $ lift $ gF
			s <- mapM getStat $ contents
			mys <- getStat $ fromJust f
			-- TODO ..
			--return $ map (Msg TRstat t . Rstat . return) $ mys:s
			--return $ return $ Msg TRstat t $ Rstat $ return $ mys
			return $ return $ Msg TRstat t $ Rstat $ mys:s

rclunk :: Msg -> Nine [Msg]
rclunk (Msg _ t (Tclunk fid)) = do
	S.modify (M.delete fid)
	return $ return $ Msg TRclunk t $ Rclunk

rauth :: Msg -> Nine [Msg]
rauth (Msg {}) = do
	throwError ENoAuthRequired

ropen :: Msg -> Nine [Msg]
ropen (Msg _ t (Topen fid mode)) = do
	-- TODO check perms
	f <- fidLookup fid
	q <- makeQid $ f
	-- TODO iounit
	return $ return $ Msg TRopen t $ Ropen q 0

rread :: Msg -> Nine [Msg]
rread (Msg _ t (Tread fid offset count)) = undefined
