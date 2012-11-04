-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Higher-level file patterns.

module Network.NineP.File
	( chanFile
	) where

import Control.Concurrent.Chan
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Word
import Prelude hiding (read)

import Network.NineP.Error
import Network.NineP.Internal.File

readCF :: Chan ByteString -> Word64 -> Word32 -> ErrorT NineError IO ByteString
readCF c offset count = case offset of
	0 -> do
		d <- lift $ readChan c
		return $ B.take (fromIntegral count) $ d
	_ -> throwError $ OtherError "can't read at offset"

writeCF :: Chan ByteString -> Word64 -> ByteString -> ErrorT NineError IO Word32
writeCF c offset d = case offset of
	0 -> do
		lift $ writeChan c d
		return $ fromIntegral $ B.length d
	_ -> throwError $ OtherError "can't write at offset"

-- |A file that reads from and writes to the specified Chans
chanFile :: String -> Maybe (Chan ByteString) -> Maybe (Chan ByteString) -> NineFile
chanFile name rc wc = (boringFile name) {
	read = maybe (read $ boringFile "") readCF rc,
	write = maybe (write $ boringFile "") writeCF wc}
