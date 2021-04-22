{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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
	, rcreate
	, rread
	, rwrite
	, rremove
	, rflush
	) where

import Control.Concurrent.MState hiding (put)
import Control.Exception
import Control.Monad.EmbedIO
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

checkPerms :: (Monad m, EmbedIO m) => Word16 -> NineFile m -> Word8 -> Nine m ()
checkPerms tag f want = do
	s <- getStat tag f
	checkPerms' (st_mode s) want

checkPerms' :: (Monad m, EmbedIO m) => Word32 -> Word8 -> Nine m ()
checkPerms' have want = do
	-- TODO stop presuming we are owners
	let checkRead = unless (testBit have 2) $ throw EPermissionDenied
	let checkWrite = unless (testBit have 1) $ throw EPermissionDenied
	let checkExec = unless (testBit have 0) $ throw EPermissionDenied
	when (testBit want 4) $ do
		checkWrite
		throw $ ENotImplemented "OTRUNC"
	when (testBit want 6) $ do
		throw $ ENotImplemented "ORCLOSE"
	case want of
		0 -> checkRead
		1 -> checkWrite
		2 -> checkRead >> checkWrite
		3 -> checkExec

getQidTyp :: Stat -> Word8
getQidTyp s = fromIntegral $ shift (st_mode s) 24

makeQid :: (Monad m, EmbedIO m) => Word16 -> NineFile m -> Nine m Qid
makeQid t x = do
	s <- getStat t x
	return $ Qid (getQidTyp s) 0 42

rversion :: Msg -> Nine m [Msg]
rversion (Msg _ t (Tversion s v)) = do
	let ver = readVersion v
	modifyM_ (\st -> st { msize = s, protoVersion = ver })
	-- FIXME clunk everything and abort all outstanding I/O
	return $ return $ Msg TRversion t $ Rversion s $ show ver

rattach (Msg _ t (Tattach fid _ _ _)) = do
	root <- asks root
	insert fid root
	q <- makeQid t root
	return $ return $ Msg TRattach t $ Rattach q 

desc :: (Monad m, EmbedIO m) => NineFile m -> String -> m (NineFile m)
desc f ".." = do
	mp <- parent f
	return $ case mp of
		Just p -> p
		Nothing -> f
desc f s = descend f s

walk :: (Monad m, EmbedIO m) => [Qid] -> Word16 -> [String] -> NineFile m -> Nine m (NineFile m, [Qid])
walk qs t [] f = return (f, qs)
walk qs t (x:xs) (RegularFile {}) = throw ENotADir
walk qs t (x:xs) d@(Directory {}) = do
	f <- call t $ desc d x
	q <- makeQid t f
	walk (q:qs) t xs f

walk' :: (Monad m, EmbedIO m) => Word16 -> [String] -> NineFile m -> Nine m (NineFile m, [Qid])
walk' = walk []

rwalk (Msg _ t (Twalk fid newfid path)) = do
	f <- lookup fid
	(nf, qs) <- walk' t path f
	insert newfid nf
	return $ return $ Msg TRwalk t $ Rwalk $ qs

getStat :: (Monad m, EmbedIO m) => Word16 -> NineFile m -> Nine m Stat
getStat t f = do
	let fixDirBit = (case f of
				(RegularFile {}) -> flip clearBit 31
				(Directory {}) -> flip setBit 31
			)
	s <- call t $ stat f
	return s { st_mode = fixDirBit $ st_mode s,
		st_qid = (st_qid s) { qid_typ = getQidTyp s } }
	
rstat (Msg _ t (Tstat fid)) = do
	f <- lookup fid
	case f of
		RegularFile {} -> do
			s <- getStat t f
			return $ return $ Msg TRstat t $ Rstat $ [s]
		Directory {} -> do
			mys <- getStat t f
			return $ return $ Msg TRstat t $ Rstat $ return $ mys

rclunk (Msg _ t (Tclunk fid)) = do
	delete fid
	return $ return $ Msg TRclunk t $ Rclunk

rauth (Msg {}) = do
	throw ENoAuthRequired

open :: (Monad m, EmbedIO m) => Word16 -> NineFile m -> Nine m Qid
open t f = do
	makeQid t f

ropen (Msg _ t (Topen fid mode)) = do
	f <- lookup fid
	checkPerms t f mode
	qid <- open t f
	iou <- iounit
	return $ return $ Msg TRopen t $ Ropen qid iou

rcreate (Msg _ t (Tcreate fid name perm mode)) = do
	f <- lookup fid
	-- TODO check permissions to create
	case f of
		RegularFile {} -> throw ENotADir
		Directory {} -> do
			nf <- call t $ (create f) name perm
			insert fid nf
			qid <- open t f
			iou <- iounit
			return $ return $ Msg TRcreate t $ Rcreate qid iou

rread :: (Monad m, EmbedIO m) => Msg -> Nine m [Msg]
rread (Msg _ t (Tread fid offset count)) = do
	f <- lookup fid
	u <- iounit
	checkPerms t f 0
	let	splitMsg d s = let r = splitMsg' d s in if null r then [B.empty] else r
		splitMsg' d s = if B.null d then [] else
			let (a, b) = B.splitAt s d in a : splitMsg' b s
	case f of
		RegularFile {} -> do
			d <- call t $ (read f) offset count
			mapM (return . Msg TRread t . Rread) $ splitMsg d $ fromIntegral u
		Directory {} -> do
			contents <- call t $ getFiles f
			s <- mapM (getStat t) $ contents
			let d = runPut $ mapM_ put s
			mapM (return . Msg TRread t . Rread) $ splitMsg (B.drop (fromIntegral offset) d) $ fromIntegral u
		
--rwrite :: Msg -> Nine m [Msg]
rwrite (Msg _ t (Twrite fid offset d)) = do
	f <- lookup fid
	checkPerms t f 1
	case f of
		Directory {} -> throw EDir
		RegularFile {} -> do
			c <- call t $ (write f) offset d
			return $ return $ Msg TRwrite t $ Rwrite c

rwstat (Msg _ t (Twstat fid stat)) = do
	-- TODO check perms
	f <- lookup fid
	-- TODO implement
	return $ return $ Msg TRwstat t $ Rwstat

rremove (Msg _ t (Tremove fid)) = do
	-- TODO check perms
	f <- lookup fid
	throw $ ENotImplemented "remove"

-- TODO meaningful flush behaviour instead of pretending it works
rflush (Msg _ t _) = return $ return $ Msg TRflush t $ Rflush
