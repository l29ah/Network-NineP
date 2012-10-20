{-# LANGUAGE OverloadedStrings #-}

module HighNineP where

import qualified Data.ByteString.Char8 as B
import FidMap
import Data.Map
import Data.Word

data Qid = Qid Word8 Word32 Word64
data NineStat = NineStat {
		sType :: Word16,
		sDev :: Word32,
		sQid :: Qid,
		sMode :: Word32,
		sATime :: Word32,
		sMTime :: Word32,
		sLength :: Word64,
		sName :: B.ByteString,
		sUID :: B.ByteString,
		sGID :: B.ByteString,
		sMUID :: B.ByteString
	}

data NineFile =
	RegularFile {
        	nineRead :: Word32 -> Word32 -> IO (B.ByteString),
        	nineWrite :: Word64 -> B.ByteString -> IO (Word32),
		remove :: IO (),
		stat :: IO NineStat,
		wstat :: NineStat -> IO ()
	} | Directory {
		getFiles :: IO (Map B.ByteString NineFile),
		remove :: IO (),
		stat :: IO NineStat,
		wstat :: NineStat -> IO ()
	}

data NineClient = NineClient {
        fidMap :: FidMap NineFile
}

boringStat :: NineStat
boringStat = NineStat 0 0 (Qid 0 0 0) 0 0 0 0 "boring" "" "" ""
