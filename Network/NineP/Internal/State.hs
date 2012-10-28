module Network.NineP.Internal.State
	( Nine
	, Config(..)
	, lookup
	, insert
	, delete
	) where

import Control.Monad.RWS (RWST(..), evalRWST)
import qualified Control.Monad.State.Class as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word
import Prelude hiding (lookup)

import Network.NineP.Error
import Network.NineP.Internal.File

data Config = Config {
		root :: NineFile
	}

type Nine x = ErrorT NineError (RWST Config () (Map Word32 NineFile) IO) x 

lookup :: Word32 -> Nine NineFile
lookup fid = do
	m <- S.get
	case M.lookup fid m of
		Nothing -> throwError $ ENoFid fid
		Just f -> return f

insert :: Word32 -> NineFile -> Nine ()
insert fid f = do
	m <- S.get
	S.modify (M.insert fid f)

delete :: Word32 -> Nine ()
delete fid = do
	S.modify $ M.delete fid
