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

import Control.Monad.RWS (RWST(..), evalRWST)
import Control.Monad.Reader.Class (asks)
import qualified Control.Monad.State.Class as S
import Data.Accessor.Monad.Trans.RWS hiding (lift)
import Data.Binary.Put
import Data.Bits
import Data.NineP
import Data.Map (Map)
import Data.Maybe
import Data.Word
import Prelude hiding (lookup)

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
rversion (Msg _ t (Tversion s _)) = do
	lift $ set msize s
	return $ return $ Msg TRversion t (Rversion s "9P2000")

rattach :: Msg -> Nine [Msg]
rattach (Msg _ t (Tattach fid _ _ _)) = do
	root <- asks root
	insert fid root
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
	s <- lift $ lift $ stat f
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

ropen :: Msg -> Nine [Msg]
ropen (Msg _ t (Topen fid mode)) = do
	f <- lookup fid
	checkPerms f mode
	q <- makeQid $ f
	-- TODO iounit
	return $ return $ Msg TRopen t $ Ropen q 0

rread :: Msg -> Nine [Msg]
rread (Msg _ t (Tread fid offset count)) = do
	f <- lookup fid
	checkPerms f 0
	case f of
		RegularFile {} -> undefined
		Directory {} -> do
			--when (offset > 0) $ throwError $ ENotImplemented "directory read at offset"
			if (offset > 0)
				then return $ return $ Msg TRread t $ Rread ""
				else do
					contents <- lift $ lift $ getFiles f
					s <- mapM getStat $ contents
					mys <- getStat f
					let d = runPut $ mapM_ put $ mys:s
					-- TODO ..
					-- TODO split
					return $ return $ Msg TRread t $ Rread d
		
rwrite :: Msg -> Nine [Msg]
rwrite (Msg _ t (Twrite fid offset d)) = do
	f <- lookup fid
	checkPerms f 1
	case f of
		Directory {} -> throwError EDir
		RegularFile {} -> throwError $ ENotImplemented "write"

rwstat :: Msg -> Nine [Msg]
rwstat (Msg _ t (Twstat fid stat)) = do
	-- TODO check perms
	f <- lookup fid
	undefined

rremove :: Msg -> Nine [Msg]
rremove (Msg _ t (Tremove fid)) = do
	-- TODO check perms
	f <- lookup fid
	undefined

rcreate :: Msg -> Nine [Msg]
rcreate (Msg _ t (Tcreate fid name perm mode)) = undefined

rflush :: Msg -> Nine [Msg]
rflush _ = return []
