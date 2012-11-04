{-# LANGUAGE TemplateHaskell #-}

module Network.NineP.Internal.State
	( Nine
	, NineVersion(..)
	, readVersion
	, Config(..)
	, emptyState
	, msize
	, protoVersion
	, lookup
	, insert
	, delete
	, iounit
	) where

import Control.Monad.RWS (RWST(..), evalRWST)
import Data.Accessor
import Data.Accessor.Monad.Trans.RWS hiding (lift)
import Data.Accessor.Template
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
data Config = Config {
		-- |The @/@ directory of the hosted filesystem
		root :: NineFile,
		-- |The listening address. The syntax is taken from @Plan 9@ operating system and has the form @unix!/path/to/socket@ for unix socket files, and @tcp!hostname!port@ for tcp sockets.
		addr :: String
	}

data NineState = NineState {
		fidMap_ :: Map Word32 NineFile,
		msize_ :: Word32,
		protoVersion_ :: NineVersion
	}

emptyState = NineState
	(M.empty :: Map Word32 NineFile)
	0
	VerUnknown

$( deriveAccessors ''NineState )

type Nine x = ErrorT NineError (RWST Config () NineState IO) x 

lookup :: Word32 -> Nine NineFile
lookup fid = do
	m <- lift $ get fidMap
	case M.lookup fid m of
		Nothing -> throwError $ ENoFid fid
		Just f -> return f

insert :: Word32 -> NineFile -> Nine ()
insert fid f = do
	m <- lift $ get fidMap
	lift $ modify fidMap $ M.insert fid f

delete :: Word32 -> Nine ()
delete fid = do
	lift $ modify fidMap $ M.delete fid

iounit :: Nine Word32
iounit = do
	ms <- lift $ get msize
	-- 23 is the biggest size of a message (write), but libixp uses 24, so we do too to stay safe
	return $ ms - 24
