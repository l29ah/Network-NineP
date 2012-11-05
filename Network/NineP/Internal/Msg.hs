{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Internal.Msg
	( Config(..)
	, rversion
	, rattach
	, rwalk
	, rstat
	, rwstat
	, rclunk
	, rauth
	, ropen
	, rread
	, rwrite
	, rremove
	, rcreate
	, rflush
	) where

import Control.Concurrent.MState hiding (put)
import Control.Monad.Reader
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.NineP
import Data.Map (Map)
import Data.Maybe
import Data.Word
import Prelude hiding (lookup, read)

import Network.NineP.Error
import Network.NineP.Internal.File
import Network.NineP.Internal.State

checkPerms :: NineFile -> Word8 -> Nine ()
checkPerms f want = do
	s <- getStat f
	checkPerms' (st_mode s) want

checkPerms' :: Word32 -> Word8 -> Nine ()
checkPerms' have want = do
	-- TODO stop presuming we are owners
	let checkRead = unless (testBit have 2) $ throwError EPermissionDenied
	let checkWrite = unless (testBit have 1) $ throwError EPermissionDenied
	let checkExec = unless (testBit have 0) $ throwError EPermissionDenied
	when (testBit want 4) $ do
		checkWrite
		throwError $ ENotImplemented "OTRUNC"
	when (testBit want 6) $ do
		throwError $ ENotImplemented "ORCLOSE"
	case want of
		0 -> checkRead
		1 -> checkWrite
		2 -> checkRead >> checkWrite
		3 -> checkExec

getQidTyp :: Stat -> Word8
getQidTyp s = fromIntegral $ shift (st_mode s) 24

makeQid :: NineFile -> Nine Qid
makeQid x = do
	s <- getStat x
	return $ Qid (getQidTyp s) 0 42

rversion :: Msg -> Nine [Msg]
rversion (Msg _ t (Tversion s v)) = do
	let ver = readVersion v
	lift $ modifyM_ (\st -> st { msize = s, protoVersion = ver })
	return $ return $ Msg TRversion t $ Rversion s $ show ver

rattach :: Msg -> Nine [Msg]
rattach (Msg _ t (Tattach fid _ _ _)) = do
	root <- asks root
	insert fid root
	q <- makeQid root
	return $ return $ Msg TRattach t $ Rattach q 

desc :: NineFile -> String -> ErrorT NineError IO NineFile
desc f ".." = do
	mp <- lift $ parent f
	return $ case mp of
		Just p -> p
		Nothing -> f
desc f s = descend f s

walk :: [Qid] -> [String] -> NineFile -> Nine (NineFile, [Qid])
walk qs [] f = return (f, qs)
walk qs (x:xs) (RegularFile {}) = throwError ENotADir
walk qs (x:xs) d@(Directory {}) = do
	f <- mapErrorT (lift . lift) $ desc d x
	q <- makeQid f
	walk (q:qs) xs f

walk' = walk []

rwalk :: Msg -> Nine [Msg]
rwalk (Msg _ t (Twalk fid newfid path)) = do
	f <- lookup fid
	(nf, qs) <- walk' path f
	insert newfid nf
	return $ return $ Msg TRwalk t $ Rwalk $ qs

getStat :: NineFile -> Nine Stat
getStat f = do
	let fixDirBit = (case f of
				(RegularFile {}) -> flip clearBit 31
				(Directory {}) -> flip setBit 31
			)
	s <- lift $ lift $ lift $ stat f
	return s { st_mode = fixDirBit $ st_mode s,
		st_qid = (st_qid s) { qid_typ = getQidTyp s } }
	
rstat :: Msg -> Nine [Msg]
rstat (Msg _ t (Tstat fid)) = do
	f <- lookup fid
	case f of
		RegularFile {} -> do
			s <- getStat f
			return $ return $ Msg TRstat t $ Rstat $ [s]
		Directory {} -> do
			mys <- getStat f
			return $ return $ Msg TRstat t $ Rstat $ return $ mys

rclunk :: Msg -> Nine [Msg]
rclunk (Msg _ t (Tclunk fid)) = do
	delete fid
	return $ return $ Msg TRclunk t $ Rclunk

rauth :: Msg -> Nine [Msg]
rauth (Msg {}) = do
	throwError ENoAuthRequired

open :: NineFile -> Nine Qid
open f = do
	makeQid $ f

ropen :: Msg -> Nine [Msg]
ropen (Msg _ t (Topen fid mode)) = do
	f <- lookup fid
	checkPerms f mode
	q <- open f
	iou <- iounit
	return $ return $ Msg TRopen t $ Ropen q iou

rread :: Msg -> Nine [Msg]
rread (Msg _ t (Tread fid offset count)) = do
	f <- lookup fid
	u <- iounit
	checkPerms f 0
	let	splitMsg d s = let r = splitMsg' d s in if null r then [B.empty] else r
		splitMsg' d s = if B.null d then [] else
			let (a, b) = B.splitAt s d in a : splitMsg' b s
	case f of
		RegularFile {} -> do
			d <- mapErrorT (lift . lift) $ (read f) offset count
			mapM (return . Msg TRread t . Rread) $ splitMsg d $ fromIntegral u
		Directory {} -> do
			contents <- lift $ lift $ lift $ getFiles f
			s <- mapM getStat $ contents
			let d = runPut $ mapM_ put s
			mapM (return . Msg TRread t . Rread) $ splitMsg (B.drop (fromIntegral offset) d) $ fromIntegral u
		
rwrite :: Msg -> Nine [Msg]
rwrite (Msg _ t (Twrite fid offset d)) = do
	f <- lookup fid
	checkPerms f 1
	case f of
		Directory {} -> throwError EDir
		RegularFile {} -> do
			c <- mapErrorT (lift . lift) $ (write f) offset d
			return $ return $ Msg TRwrite t $ Rwrite c

rwstat :: Msg -> Nine [Msg]
rwstat (Msg _ t (Twstat fid stat)) = do
	-- TODO check perms
	f <- lookup fid
	throwError $ ENotImplemented "wstat"

rremove :: Msg -> Nine [Msg]
rremove (Msg _ t (Tremove fid)) = do
	-- TODO check perms
	f <- lookup fid
	throwError $ ENotImplemented "remove"

rcreate :: Msg -> Nine [Msg]
rcreate (Msg _ t (Tcreate fid name perm mode)) = do
	-- TODO check perms
	--dir <- lookup fid
	if testBit mode 31
		then do 
			throwError $ ENotImplemented "create directory"
		else do
			throwError $ ENotImplemented "create file"
	--q <- open f
	-- TODO iounit
	--return $ return $ Msg TRcreate t $ Rcreate

rflush :: Msg -> Nine [Msg]
rflush _ = return []
