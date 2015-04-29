-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Higher-level file patterns. Don't support read/write operations at offsets and handling stat changes as for now.

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}

module Network.NineP.File
	( isDir
	, simpleFile
	, simpleFileBy
	, simpleDirectory
	, rwFile
	, memoryFile
	, memoryDirectory
	) where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.EmbedIO
import Control.Monad.Trans
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Bits
import Data.Convertible.Base
import Data.IORef
import Data.StateRef
import Data.Word
import Prelude hiding (read)

import Network.NineP.Error
import Network.NineP.Internal.File

-- |Tests if the file is a directory
isDir :: Word32	-- ^Permissions
	-> Bool
isDir perms = testBit perms 31

simpleRead :: (Monad m, EmbedIO m) => m ByteString -> Word64 -> Word32 -> m ByteString
simpleRead get offset count = do
		r <- get
		(return . B.take (fromIntegral count) . B.drop (fromIntegral offset)) r

-- TODO make it offset-aware
simpleWrite :: (Monad m, EmbedIO m) => (ByteString -> m ()) -> Word64 -> ByteString -> m Word32
simpleWrite put offset d = case offset of
	_ -> do
		r <- put d
		(const $ return $ fromIntegral $ B.length d) r
	--_ -> throwError $ OtherError "can't write at offset"


-- TODO remove
-- |A file that reads and writes using simple user-specified callbacks.
rwFile :: forall m. (EmbedIO m)
		=> String	-- ^File name
		-> Maybe (m ByteString)	-- ^Read handler
		-> Maybe (ByteString -> m ())	-- ^Write handler
		-> NineFile m
rwFile name rc wc = simpleFileBy name (maybe (fst nulls) id rc, maybe (snd nulls) id wc) (id, id)

-- FIXME sane errors
-- |Placeholder source and sink
nulls :: MonadIO m => (m a, a -> m ())
nulls = (throw $ Underflow, const $ return ())

-- |A file that reads from and writes to the supplied 'Ref' instances, with converstion to the appropriate types. See 'Network.NineP.File.Instances', 'Data.Convertible.Instances' and 'Data.StateRef.Instances'. Use '()', if the file is meant to be read-only/write-only.
simpleFile :: forall a b m rr wr. (Monad m, EmbedIO m, ReadRef rr m a, Convertible a ByteString, WriteRef wr m b, Convertible ByteString b)
		=> String	-- ^File name
		-> rr	-- ^Reading function
		-> wr	-- ^Writing function
		-> NineFile m
simpleFile name rr wr = simpleFileBy name (readReference rr, writeReference wr) (convert, convert)

-- Shouldn't we make it simpler?
-- |Typeclass-free version of 'simpleFile'.
simpleFileBy :: forall a b m. (Monad m, EmbedIO m)
		=> String	-- ^File name
		-> (m a, b -> m ())	-- ^Reading and writing handle
		-> (a -> ByteString, ByteString -> b)	-- ^Type conversion handles
		-> NineFile m
simpleFileBy name (rd, wr) (rdc, wrc) = (boringFile name :: NineFile m) {
		read = simpleRead $ liftM rdc $ rd,
		write = simpleWrite $ wr . wrc 
	}

-- |A file that stores its contents in the form of 'IORef' 'ByteString'
memoryFile :: forall m. (Monad m, EmbedIO m)
	=> String	-- ^File name
	-> IO (NineFile m)
memoryFile name = do
	c <- newIORef "" :: IO (IORef ByteString)
	return $ simpleFileBy name (
			liftIO $ readIORef c,
			liftIO . writeIORef c
		) (id, id)

-- |A directory that stores its contents in the form of 'IORef [(String, NineFile m)]'
simpleDirectory :: forall m. (Monad m, EmbedIO m)
			=> String	-- ^File name
			-> (String -> m (NineFile m))	-- ^A function for creating new files
			-> (String -> m (NineFile m))	-- ^A function for creating new directories
			-> IO (NineFile m)
simpleDirectory name newfile newdir = do
	files <- newIORef [] :: IO (IORef [(String, NineFile m)])
	return (boringDir name [] :: NineFile m) {
		create = \name perms -> do
			nf <- (if isDir perms then newdir else newfile) name
			let nelem = (name, nf)
			liftIO $ atomicModifyIORef' files (\l -> (nelem:l, ()))
			return nf,
		getFiles = liftIO $ liftM (map snd) $ readIORef files,
		descend = \name -> do
			d <- liftIO $ readIORef files
			maybe (throw $ ENoFile name) (return) $ lookup name d
	}

-- |A composition of a 'simpleDirectory' and a 'memoryFile'
memoryDirectory :: forall m. (Monad m, EmbedIO m)
			=> String	-- ^File name
			-> IO (NineFile m)
memoryDirectory name = simpleDirectory name (liftIO . memoryFile) (liftIO . memoryDirectory)
