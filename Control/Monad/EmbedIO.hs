-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Provides one with the ability to pass her own monads in the callbacks.
{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}
module Control.Monad.EmbedIO where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Prelude hiding (catch)

class (MonadIO o) => EmbedIO o where
	type Content o
	embed :: (Content o -> IO a) -> o a
	callback :: o a -> Content o -> IO a

data Void

instance EmbedIO IO where
	type Content IO = Void
	embed f = f undefined
	callback action _ = action

bracketE :: EmbedIO m => m r -> (r -> m b) -> (r -> m a) -> m a
bracketE before after during =
	embed $ \x -> bracket (before' x) (\a -> after' a x) (\a -> during' a x)
	where
	before' x = callback before x
	after' a x = callback (after a) x
	during' a x = callback (during a) x

catchE :: (EmbedIO m, Exception e) => m a -> (e -> m a) -> m a
catchE action handler = embed $ \x -> catch (action' x) (\e -> handler' e x)
	where
	action' x = callback action x
	handler' e x = callback (handler e) x

handleE :: (EmbedIO m, Exception e) => (e -> m a) -> m a -> m a
handleE = flip catchE

tryE :: (EmbedIO m, Exception e) => m a -> m (Either e a)
tryE action = embed $ \x -> try (callback action x)

throwE :: (EmbedIO m, Exception e) => e -> m a
throwE = liftIO . throwIO

forkE :: EmbedIO m => m () -> m ThreadId
forkE action = embed $ \x -> forkIO (callback action x)
