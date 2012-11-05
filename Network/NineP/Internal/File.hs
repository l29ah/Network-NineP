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
        	read :: Word64	-- Offset
			-> Word32 -- Length
			-> ErrorT NineError IO (B.ByteString),	-- Resulting data
        	write :: Word64	-- Offset
			-> B.ByteString	-- The written data
			-> ErrorT NineError IO (Word32),	-- Number of bytes written successfully. Should return @0@ in case of an error.
		remove :: IO (),
		stat :: IO Stat,
		wstat :: Stat -> IO (),
		version :: IO Word32
	} | Directory {
		-- |A callback to get the list of the files this directory contains. Must not contain @.@ and @..@ entries.
		getFiles :: IO [NineFile],
		parent :: IO (Maybe NineFile),
		-- |A callback to address a specific file by its name. @.@ and @..@ are handled in the library.
		descend :: String -> ErrorT NineError IO NineFile,
		remove :: IO (),
		-- |The directory stat must return only stat for @.@.
		stat :: IO Stat,
		wstat :: Stat -> IO (),
		version :: IO Word32
	}

boringStat :: Stat
boringStat = Stat 0 0 (Qid 0 0 0) 0o0777 0 0 0 "boring" "root" "root" "root"

boringFile :: String -> NineFile
boringFile name = RegularFile
        (\_ _ -> return "")
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
        stat = (return $ boringStat {st_name = name}),
        wstat = (const $ return ()),
	version = (return 0),
	parent = return Nothing }

