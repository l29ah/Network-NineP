-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Higher-level file patterns. Don't support read/write operations at offsets and handling stat changes as for now.

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}

module Network.NineP.File
	( rwFile
	, IOObject
	, DataTypeObject
	, simpleFile
	, heterObj
	, nulls
	, chans
	, lazyByteStrings
	, booleans
	) where

import Control.Concurrent.Chan
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
-- TODO remove
rwFile :: forall m. (EmbedIO m)
		=> String	-- ^File name
		-> Maybe (m ByteString)	-- ^Read handler
		-> Maybe (ByteString -> m ())	-- ^Write handler
		-> NineFile m
rwFile name rc wc = (boringFile name :: NineFile m) {
		read = maybe (read $ (boringFile "" :: NineFile m)) (simpleRead_) rc,
		write = maybe (write $ (boringFile "" :: NineFile m)) (simpleWrite_) wc
	}

-- TODO docs
type IOObject a = (IO a, a -> IO ())

-- FIXME sane errors
heterObj :: IOObject a -> IOObject a -> IOObject a
heterObj a b = (fst a, snd b)

nulls :: IOObject a
nulls = (throw $ Underflow, const $ return ())

chans :: Chan a -> IOObject a
chans a = (readChan a, writeChan a)

type DataTypeObject a = (a -> ByteString, ByteString -> a)

lazyByteStrings :: DataTypeObject ByteString 
lazyByteStrings = (id, id)

showBool True = "true"
showBool False = "false"
readBool s
	| s == "1" = True
	| s == "true" = True
	| s == "0" = False
	| s == "false" = False
booleans :: DataTypeObject Bool
booleans = (showBool, readBool)

simpleFile :: forall a m. (Monad m, EmbedIO m)
		=> String
		-> IOObject a
		-> DataTypeObject a
		-> NineFile m
simpleFile name (rd, wr) (rdc, wrc) = (boringFile name :: NineFile m) {
		read = simpleRead $ liftM rdc $ rd,
		write = simpleWrite $ wr . wrc 
	}
