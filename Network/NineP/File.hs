-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Higher-level file patterns. Don't support read/write operations at offsets and handling stat changes as for now.

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Network.NineP.File
	( chanFile
	, mVarFile
	, rwFile
	) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.EmbedIO
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Word
import Prelude hiding (read)

import Network.NineP.Error
import Network.NineP.Internal.File

simpleRead_ :: (Monad m, EmbedIO m) => m ByteString -> Word64 -> Word32 -> ErrorT NineError m ByteString
simpleRead_ get offset count = case offset of
	_ -> do
		r <- lift $ tryE $ get
		either (throwError . OtherError . (show :: SomeException -> String))
				(return . B.take (fromIntegral count)) r
	--_ -> throwError $ OtherError "can't read at offset"

simpleWrite_ :: (Monad m, EmbedIO m) => (ByteString -> m ()) -> Word64 -> ByteString -> ErrorT NineError m Word32
simpleWrite_ put offset d = case offset of
	_ -> do
		r <- lift $ tryE $ put d
		either (throwError . OtherError . (show :: SomeException -> String))
				(const $ return $ fromIntegral $ B.length d) r
	--_ -> throwError $ OtherError "can't write at offset"

simpleRead :: (Monad m, EmbedIO m) => IO ByteString -> Word64 -> Word32 -> ErrorT NineError m ByteString
simpleRead get offset count = case offset of
	_ -> do
		d <- lift $ liftIO $ get
		return $ B.take (fromIntegral count) $ d
	--_ -> throwError $ OtherError "can't read at offset"

simpleWrite :: (Monad m, EmbedIO m) => (ByteString -> IO ()) -> Word64 -> ByteString -> ErrorT NineError m Word32
simpleWrite put offset d = case offset of
	_ -> do
		lift $ liftIO $ put d
		return $ fromIntegral $ B.length d
	--_ -> throwError $ OtherError "can't write at offset"

-- |A file that reads and writes using simple user-specified callbacks
rwFile :: forall m. (EmbedIO m)
		=> String	-- ^File name
		-> Maybe (m ByteString)	-- ^Read handler
		-> Maybe (ByteString -> m ())	-- ^Write handler
		-> NineFile m
rwFile name rc wc = (boringFile name :: NineFile m) {
		read = maybe (read $ (boringFile "" :: NineFile m)) (simpleRead_) rc,
		write = maybe (write $ (boringFile "" :: NineFile m)) (simpleWrite_) wc
	}

-- |A file that reads from and writes to the specified Chans
chanFile :: forall m. (Monad m, EmbedIO m)
		=> String	-- ^File name
		-> Maybe (Chan ByteString)	-- ^@Chan@ to read from
		-> Maybe (Chan ByteString)	-- ^@Chan@ to write to
		-> NineFile m
chanFile name rc wc = (boringFile name :: NineFile m) {
		read = maybe (read $ (boringFile "" :: NineFile m)) (simpleRead . readChan) rc,
		write = maybe (write $ (boringFile "" :: NineFile m)) (simpleWrite . writeChan) wc
	}

-- |A file that reads from and writes to the specified MVars
mVarFile :: forall m. (Monad m, EmbedIO m)
		=> String	-- ^File name
		-> Maybe (MVar ByteString)	-- ^@MVar@ to read from
		-> Maybe (MVar ByteString)	-- ^@MVar@ to write to
		-> NineFile m
mVarFile name rc wc = (boringFile name :: NineFile m) {
		read = maybe (read $ (boringFile "" :: NineFile m)) (simpleRead . takeMVar) rc,
		write = maybe (write $ (boringFile "" :: NineFile m)) (simpleWrite . putMVar) wc
	}

-- |A file that reads from and writes to the specified TVars
tVarFile :: forall m. (Monad m, EmbedIO m)
		=> String	-- ^File name
		-> Maybe (TVar ByteString)	-- ^@TVar@ to read from
		-> Maybe (TVar ByteString)	-- ^@TVar@ to write to
		-> NineFile m
tVarFile name rc wc = (boringFile name :: NineFile m) {
		read = maybe (read $ (boringFile "" :: NineFile m)) (\x -> simpleRead $ atomically $ readTVar x) rc,
		write = maybe (write $ (boringFile "" :: NineFile m)) (\x -> simpleWrite $ atomically . writeTVar x) wc
	}
