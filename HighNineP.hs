{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -pgmP cpp #-}

module HighNineP 
	( NineFile(..)
	, Config(..)
	, run9PServer
	, boringFile
	, boringDir
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

import Error

import Debug.Trace

data NineFile =
	RegularFile {
        	read :: Word64 -> Word32 -> IO (B.ByteString),
        	write :: Word64 -> B.ByteString -> IO (Word32),
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO (),
		version :: IO Word32
	} | Directory {
		getFiles :: IO [NineFile],	-- must include ..
		parent :: IO (Maybe NineFile),
		descend :: String -> ErrorT NineError IO NineFile,
		remove :: IO (),
		stat :: IO Stat,	-- The directory stat must return only stat for .
		wstat :: Stat -> IO (),
		version :: IO Word32
	}

data Config = Config {
		root :: NineFile
	}

type Nine x = ErrorT NineError (RWST Config () (Map Word32 NineFile) IO) x

boringStat :: Stat
boringStat = Stat 0 0 (Qid 0 0 0) 0o0777 0 0 0 "boring" "root" "root" "root"

boringFile :: String -> NineFile
boringFile name = RegularFile
        (\_ c -> return $ B.take (fromIntegral c) "i am so very boring")
        (\_ _ -> return 0)
        (return ())
        (return $ boringStat {st_name = name})
        (const $ return ())
	(return 0)

boringDir :: String -> [(String, NineFile)] -> NineFile
boringDir name contents = let m = M.fromList contents in Directory {
	getFiles = (return $ map snd $ contents),
	descend = (\x -> case M.lookup x m of
		Nothing -> throwError $ ENoFile x
		Just f -> return f),
        remove = (return ()),
        stat = (return $ boringStat {st_name = "."}),
        wstat = (const $ return ()),
	version = (return 0)}

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
	st <- forkIO $ sender (readChan chan) (B.hPut h)
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
	 			)
			case r of
				(Right response) -> liftIO $ say $ response
				(Left fail) -> liftIO $ say $ Msg TRerror t $ Rerror $ show $ fail

makeQid :: NineFile -> Nine Qid
makeQid x = do
	s <- getStat x
	return $ Qid (fromIntegral $ shift (st_mode s) 24) 0 42

--TODO version check
rversion :: Msg -> Nine Msg
rversion (Msg _ t (Tversion s _)) = return $ Msg TRversion t (Rversion s "9P2000")

rattach :: Msg -> Nine Msg
rattach (Msg _ t (Tattach fid _ _ _)) = do
	root <- asks root
	S.modify (M.insert fid root)
	q <- makeQid root
	return $ Msg TRattach t $ Rattach q 

--walk :: [String] -> NineFile -> NineFile
walk :: [Qid] -> [String] -> NineFile -> Nine (NineFile, [Qid])
walk qs [] f = return (f, qs)
--walk (x:[]) (RegularFile _ _ _ _ _ _) = return f
walk qs (x:xs) (RegularFile {}) = throwError ENotADir
walk qs (x:xs) (Directory {descend = desc}) = do
	f <- (\x -> case x of
			(Left e) -> throwError e
			(Right v) -> return v
		) =<< (lift $ lift $ runErrorT $ desc x)
	--m <- lift $ lift gF
	--case M.lookup x m of
		--Nothing -> throwError $ ENoFile x
		--Just f -> do
	q <- makeQid f
	walk (q:qs) xs f

walk' = walk []

rwalk :: Msg -> Nine Msg
rwalk (Msg _ t (Twalk fid newfid path)) = do
	m <- S.get
	case M.lookup fid m of
		Nothing -> throwError $ ENoFid fid
		Just f -> do
			(nf, qs) <- walk' path f
			S.modify (M.insert newfid nf)
			return $ Msg TRwalk t $ Rwalk $ qs

getStat :: NineFile -> Nine Stat
getStat f = do
	s <- lift $ lift $ stat f
	return s { st_mode = (case f of
			(RegularFile {}) -> flip clearBit 32
			(Directory {}) -> flip setBit 32
		) $ st_mode s }
	
rstat :: Msg -> Nine Msg
rstat (Msg _ t (Tstat fid)) = do
	m <- S.get
	let f = M.lookup fid m
	case f of
		Nothing -> throwError $ ENoFid fid
		Just (RegularFile _ _ _ _ _ _) -> do
			s <- getStat $ fromJust f
			return $ Msg TRstat t $ Rstat $ [s]
		Just (Directory { getFiles = gF }) -> do
			contents <- lift $ lift $ gF
			s <- mapM getStat $ contents
			mys <- getStat $ fromJust f
			-- todo ..
			return $ Msg TRstat t $ Rstat $ mys:s

rclunk :: Msg -> Nine Msg
rclunk (Msg _ t (Tclunk fid)) = do
	S.modify (M.delete fid)
	return $ Msg TRclunk t $ Rclunk

rauth :: Msg -> Nine Msg
rauth (Msg {}) = do
	throwError ENoAuthRequired
