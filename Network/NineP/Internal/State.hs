{-# LANGUAGE TemplateHaskell #-}

module Network.NineP.Internal.State
	( Nine
	, Config(..)
	, emptyState
	, msize
	, lookup
	, insert
	, delete
	) where

import Control.Monad.RWS (RWST(..), evalRWST)
import Data.Accessor
import Data.Accessor.Monad.Trans.RWS hiding (lift)
import Data.Accessor.Template
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word
import Prelude hiding (lookup)

import Network.NineP.Error
import Network.NineP.Internal.File

data Config = Config {
		root :: NineFile
	}

data NineState = NineState {
		fidMap_ :: Map Word32 NineFile,
		msize_ :: Word32
	}

emptyState = NineState (M.empty :: Map Word32 NineFile) 0

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
