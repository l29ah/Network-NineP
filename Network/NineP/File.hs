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
	0 -> throwError $ OtherError "can't read at offset"
	_ -> do
		d <- lift $ readChan c
		return $ B.take (fromIntegral count) $ d

writeCF :: Chan ByteString -> Word64 -> ByteString -> ErrorT NineError IO Word32
writeCF c offset d = case offset of
	0 -> throwError $ OtherError "can't write at offset"
	_ -> do
		lift $ writeChan c d
		return $ fromIntegral $ B.length d

chanFile :: String -> Chan ByteString -> Chan ByteString -> NineFile
chanFile name rc wc = (boringFile name) {
	read = readCF rc,
	write = writeCF wc}
