{-# LANGUAGE FlexibleContexts #-}

module Network.NineP.Internal.State
	( Nine
	, NineVersion(..)
	, readVersion
	, Config(..)
	, NineState(..)
	, emptyState
	, lookup
	, insert
	, delete
	, iounit
	, call
	) where

import Control.Concurrent.MState
import Control.Monad.EmbedIO
import Control.Monad.Reader
import Control.Monad.State.Class
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word
import Prelude hiding (lookup)

import Network.NineP.Error
import Network.NineP.Internal.File

data NineVersion = VerUnknown | Ver9P2000

instance Show NineVersion where
	show VerUnknown = "unknown"
	show Ver9P2000 = "9P2000"

readVersion :: String -> NineVersion
readVersion s = if isPrefixOf "9P2000" s then Ver9P2000 else VerUnknown

-- |Server configuration.
data Config m = Config {
		-- |The @/@ directory of the hosted filesystem
		root :: NineFile m,
		-- |The listening address. The syntax is taken from @Plan 9@ operating system and has the form @unix!/path/to/socket@ for unix socket files, and @tcp!hostname!port@ for tcp sockets.
		addr :: String,
		monadState :: Content m
	}

data NineState m = NineState {
		fidMap :: Map Word32 (NineFile m),
		msize :: Word32,
		protoVersion :: NineVersion,
		mState :: Content m
	}

emptyState m = NineState {
	fidMap = M.empty :: Map Word32 (NineFile m),
	msize = 0,
	protoVersion = VerUnknown,
	mState = m
}

type Nine m x = ErrorT NineError (MState (NineState m) (ReaderT (Config m) IO)) x

call :: (EmbedIO m) => m a -> MState (NineState m) (ReaderT (Config m) IO) a
call x = do
	s <- (return . mState) =<< get
	lift $ lift $ callback x s

lookup :: Word32 -> Nine m (NineFile m)
lookup fid = do
	m <- (return . fidMap) =<< get 
	case M.lookup fid m of
		Nothing -> throwError $ ENoFid fid
		Just f -> return f

insert :: Word32 -> NineFile m -> Nine m ()
insert fid f = do
	m <- (return . fidMap) =<< get 
	lift $ modifyM_ (\s -> s { fidMap = M.insert fid f $ fidMap s })

delete :: Word32 -> Nine m ()
delete fid = do
	lift $ modifyM_ (\s -> s { fidMap = M.delete fid $ fidMap s })

iounit :: Nine m Word32
iounit = do
	ms <- (return . msize) =<< get
	-- 23 is the biggest size of a message (write), but libixp uses 24, so we do too to stay safe
	return $ ms - 24
