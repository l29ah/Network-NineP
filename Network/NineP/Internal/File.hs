{-# LANGUAGE OverloadedStrings #-}

module Network.NineP.Internal.File
	( NineFile(..)
	, boringFile
	, boringDir
	) where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Data.Word

import Data.NineP
import Network.NineP.Error

data NineFile =
	RegularFile {
        	read :: Word64 -> Word32 -> IO (B.ByteString),
        	write :: Word64 -> B.ByteString -> IO (Word32),
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO (),
		version :: IO Word32
	} | Directory {
		getFiles :: IO [NineFile],	-- must include ..
		parent :: IO (Maybe NineFile),
		descend :: String -> ErrorT NineError IO NineFile,
		remove :: IO (),
		stat :: IO Stat,	-- The directory stat must return only stat for .
		wstat :: Stat -> IO (),
		version :: IO Word32
	}

boringStat :: Stat
boringStat = Stat 0 0 (Qid 0 0 0) 0o0777 0 0 0 "boring" "root" "root" "root"

boringFile :: String -> NineFile
boringFile name = RegularFile
        (\_ c -> return $ B.take (fromIntegral c) "i am so very boring")
        (\_ _ -> return 0)
        (return ())
        (return $ boringStat {st_name = name})
        (const $ return ())
	(return 0)

boringDir :: String -> [(String, NineFile)] -> NineFile
boringDir name contents = let m = M.fromList contents in Directory {
	getFiles = (return $ map snd $ contents),
	descend = (\x -> case M.lookup x m of
		Nothing -> throwError $ ENoFile x
		Just f -> return f),
        remove = (return ()),
        stat = (return $ boringStat {st_name = "."}),
        wstat = (const $ return ()),
	version = (return 0),
	parent = return Nothing }

