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
import Data.Bits
import Data.NineP
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.Word

import Network.NineP.Error
import Network.NineP.Internal.File

data Config = Config {
		root :: NineFile
	}

type Nine x = ErrorT NineError (RWST Config () (Map Word32 NineFile) IO) x

getQidTyp :: Stat -> Word8
getQidTyp s = fromIntegral $ shift (st_mode s) 24

makeQid :: NineFile -> Nine Qid
makeQid x = do
	s <- getStat x
	return $ Qid (getQidTyp s) 0 42

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

rwrite :: Msg -> Nine [Msg]
rwrite (Msg _ t (Twrite fid offset d)) = undefined

rwstat :: Msg -> Nine [Msg]
rwstat (Msg _ t (Twstat fid stat)) = undefined

rremove :: Msg -> Nine [Msg]
rremove (Msg _ t (Tremove fid)) = undefined

rcreate :: Msg -> Nine [Msg]
rcreate (Msg _ t (Tcreate fid name perm mode)) = undefined

rflush :: Msg -> Nine [Msg]
rflush _ = return []
