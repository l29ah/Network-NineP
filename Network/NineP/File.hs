-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Higher-level file patterns.

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Network.NineP.File
	( chanFile
	, mVarFile
	, rwFile
	) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
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

rwFile :: forall m. (EmbedIO m) => String -> Maybe (m ByteString) -> Maybe (ByteString -> m ()) -> NineFile m
rwFile name rc wc = (boringFile name :: NineFile m) {
		read = maybe (read $ (boringFile "" :: NineFile m)) (simpleRead_) rc,
		write = maybe (write $ (boringFile "" :: NineFile m)) (simpleWrite_) wc
	}

-- |A file that reads from and writes to the specified Chans
chanFile :: forall m. (Monad m, EmbedIO m) => String -> Maybe (Chan ByteString) -> Maybe (Chan ByteString) -> NineFile m
chanFile name rc wc = (boringFile name :: NineFile m) {
		read = maybe (read $ (boringFile "" :: NineFile m)) (simpleRead . readChan) rc,
		write = maybe (write $ (boringFile "" :: NineFile m)) (simpleWrite . writeChan) wc
	}

-- |A file that reads from and writes to the specified MVars
mVarFile :: forall m. (Monad m, EmbedIO m) => String -> Maybe (MVar ByteString) -> Maybe (MVar ByteString) -> NineFile m
mVarFile name rc wc = (boringFile name :: NineFile m) {
		read = maybe (read $ (boringFile "" :: NineFile m)) (simpleRead . takeMVar) rc,
		write = maybe (write $ (boringFile "" :: NineFile m)) (simpleWrite . putMVar) wc
	}
