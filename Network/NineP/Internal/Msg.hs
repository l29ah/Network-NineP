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
import Data.Word
import Prelude hiding (lookup)

import Network.NineP.Error
import Network.NineP.Internal.File
import Network.NineP.Internal.State

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
			--contents <- lift $ lift $ getFiles f
			--s <- mapM getStat $ contents
			mys <- getStat f
			-- TODO ..
			--return $ map (Msg TRstat t . Rstat . return) $ mys:s
			--return $ return $ Msg TRstat t $ Rstat $ return $ mys
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
	-- TODO check perms
	f <- lookup fid
	q <- makeQid $ f
	-- TODO iounit
	return $ return $ Msg TRopen t $ Ropen q 0

rread :: Msg -> Nine [Msg]
rread (Msg _ t (Tread fid offset count)) = do
	f <- lookup fid
	undefined

rwrite :: Msg -> Nine [Msg]
rwrite (Msg _ t (Twrite fid offset d)) = do
	f <- lookup fid
	undefined
rwstat :: Msg -> Nine [Msg]
rwstat (Msg _ t (Twstat fid stat)) = do
	f <- lookup fid
	undefined

rremove :: Msg -> Nine [Msg]
rremove (Msg _ t (Tremove fid)) = do
	f <- lookup fid
	undefined

rcreate :: Msg -> Nine [Msg]
rcreate (Msg _ t (Tcreate fid name perm mode)) = undefined

rflush :: Msg -> Nine [Msg]
rflush _ = return []
