-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Higher-level file patterns.

module Network.NineP.File
	( chanFile
	, mVarFile
	) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Word
import Prelude hiding (read)

import Network.NineP.Error
import Network.NineP.Internal.File

simpleRead :: IO ByteString -> Word64 -> Word32 -> ErrorT NineError IO ByteString
simpleRead get offset count = case offset of
	0 -> do
		d <- lift $ get
		return $ B.take (fromIntegral count) $ d
	_ -> throwError $ OtherError "can't read at offset"

simpleWrite :: (ByteString -> IO ()) -> Word64 -> ByteString -> ErrorT NineError IO Word32
simpleWrite put offset d = case offset of
	0 -> do
		lift $ put d
		return $ fromIntegral $ B.length d
	_ -> throwError $ OtherError "can't write at offset"

-- |A file that reads from and writes to the specified Chans
chanFile :: String -> Maybe (Chan ByteString) -> Maybe (Chan ByteString) -> NineFile
chanFile name rc wc = (boringFile name) {
		read = maybe (read $ boringFile "") (simpleRead . readChan) rc,
		write = maybe (write $ boringFile "") (simpleWrite . writeChan) wc
	}

-- |A file that reads from and writes to the specified MVars
mVarFile :: String -> Maybe (MVar ByteString) -> Maybe (MVar ByteString) -> NineFile
mVarFile name rc wc = (boringFile name) {
		read = maybe (read $ boringFile "") (simpleRead . takeMVar) rc,
		write = maybe (write $ boringFile "") (simpleWrite . putMVar) wc
	}
