-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Higher-level file patterns. Don't support read/write operations at offsets and handling stat changes as for now.

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}

module Network.NineP.File
	( simpleFile
	, simpleFileBy
	, rwFile
	) where

import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.EmbedIO
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Convertible.Base
import Data.StateRef
import Data.Word
import Prelude hiding (read)

import Network.NineP.Error
import Network.NineP.Internal.File

simpleRead :: (Monad m, EmbedIO m) => m ByteString -> Word64 -> Word32 -> ErrorT NineError m ByteString
simpleRead get offset count = case offset of
	_ -> do
		r <- lift $ tryE $ get
		either (throwError . OtherError . (show :: SomeException -> String))
				(return . B.take (fromIntegral count)) r
	--_ -> throwError $ OtherError "can't read at offset"

simpleWrite :: (Monad m, EmbedIO m) => (ByteString -> m ()) -> Word64 -> ByteString -> ErrorT NineError m Word32
simpleWrite put offset d = case offset of
	_ -> do
		r <- lift $ tryE $ put d
		either (throwError . OtherError . (show :: SomeException -> String))
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
nulls :: MonadIO m => (m a, a -> m ())
nulls = (throw $ Underflow, const $ return ())

-- |A file that reads from and writes to the supplied 'Ref' instances, with converstion to the appropriate types. See 'Network.NineP.File.Instances', 'Data.Convertible.Instances' and 'Data.StateRef.Instances'. Use '()', if the file is meant to be read-only/write-only.
simpleFile :: forall a b m rr wr. (Monad m, EmbedIO m, ReadRef rr m a, Convertible a ByteString, WriteRef wr m b, Convertible ByteString b)
		=> String
		-> rr
		-> wr
		-> NineFile m
simpleFile name rr wr = simpleFileBy name (readReference rr, writeReference wr) (convert, convert)

-- |Typeclass-free version of 'simpleFile'.
simpleFileBy :: forall a b m. (Monad m, EmbedIO m)
		=> String
		-> (m a, b -> m ())
		-> (a -> ByteString, ByteString -> b)
		-> NineFile m
simpleFileBy name (rd, wr) (rdc, wrc) = (boringFile name :: NineFile m) {
		read = simpleRead $ liftM rdc $ rd,
		write = simpleWrite $ wr . wrc 
	}
