{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.NineP.Internal.File
	( NineFile(..)
	, boringFile
	, boringDir
	) where

import Control.Monad.EmbedIO
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Data.Word

import Data.NineP
import Network.NineP.Error

data NineFile m =
	RegularFile {
        	read :: Word64	-- Offset
			-> Word32 -- Length
			-> ErrorT NineError m (B.ByteString),	-- Resulting data
        	write :: Word64	-- Offset
			-> B.ByteString	-- The written data
			-> ErrorT NineError m (Word32),	-- Number of bytes written successfully. Should return @0@ in case of an error.
		remove :: m (),
		stat :: m Stat,
		wstat :: Stat -> m (),
		version :: m Word32
	} | Directory {
		-- |A callback to get the list of the files this directory contains. Must not contain @.@ and @..@ entries.
		getFiles :: m [NineFile m],
		parent :: m (Maybe (NineFile m)),
		-- |A callback to address a specific file by its name. @.@ and @..@ are handled in the library.
		descend :: String -> ErrorT NineError m (NineFile m),
		-- |Create a file under this directory.
		create :: String -> Word32 -> ErrorT NineError m (NineFile m),
		remove :: m (),
		-- |The directory stat must return only stat for @.@.
		stat :: m Stat,
		wstat :: Stat -> m (),
		version :: m Word32
	}

boringStat :: Stat
boringStat = Stat 0 0 (Qid 0 0 0) 0o0777 0 0 0 "boring" "root" "root" "root"

-- |A dumb file that can't do anything.
boringFile :: (Monad m, EmbedIO m) => String -> NineFile m
boringFile name = RegularFile
        (\_ _ -> return "")
        (\_ _ -> return 0)
        (return ())
        (return $ boringStat {st_name = name})
        (const $ return ())
	(return 0)

-- |A dumb directory that can't do anything but provide the files it contains. An user can create files, but they won't show up in listing and will essentially be 'boringFile's.
boringDir :: (Monad m, EmbedIO m) => String -> [(String, NineFile m)] -> NineFile m
boringDir name contents = let m = M.fromList contents in Directory {
	getFiles = (return $ map snd $ contents),
	descend = (\x -> case M.lookup x m of
		Nothing -> throwError $ ENoFile x
		Just f -> return f),
	create = (\name perms -> return $ boringFile name),
	remove = (return ()),
	stat = (return $ boringStat {st_name = name}),
	wstat = (const $ return ()),
	version = (return 0),
	parent = return Nothing }

