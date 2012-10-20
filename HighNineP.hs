{-# LANGUAGE OverloadedStrings #-}

module HighNineP where

import qualified Data.ByteString.Char8 as B
import FidMap
import Data.Map
import Data.Word
import Data.NineP

data NineFile =
	RegularFile {
        	nineRead :: Word64 -> Word32 -> IO (B.ByteString),
        	nineWrite :: Word64 -> B.ByteString -> IO (Word32),
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO ()
	} | Directory {
		getFiles :: IO (Map B.ByteString NineFile),
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO ()
	}

data NineClient = NineClient {
        fidMap :: FidMap NineFile
}

boringStat :: Stat
boringStat = Stat 0 0 (Qid 0 0 0) 0 0 0 0 "boring" "" "" ""
