-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Provides one with the ability to pass her own monads in the callbacks.
{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}
module Control.Monad.EmbedIO
    ( EmbedIO(..)
    , Void
    , bracketE
    , catchE
    , handleE
    , tryE
    , throwE
    , forkE
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Prelude hiding (catch)

-- |'MonadIO's that can be collapsed to and restored from a distinct value.
class (MonadIO o) => EmbedIO o where
    -- |Intermediate state storage type.
    type Content o
    -- |Propagate an 'IO' operation over the storage type to the monadic type.
    embed :: (Content o -> IO a) -> o a
    -- |Run the monadic computation using supplied state.
    callback :: o a -> Content o -> IO a

-- |Empty type. Used to represent state for 'IO' monad.
data Void

instance EmbedIO IO where
    type Content IO = Void
    embed f = f undefined
    callback action _ = action

-- |'bracket' equivalent.
bracketE :: EmbedIO m => m r -> (r -> m b) -> (r -> m a) -> m a
bracketE before after during =
    embed $ \x -> bracket (before' x) (\a -> after' a x) (\a -> during' a x)
    where
    before' x = callback before x
    after' a x = callback (after a) x
    during' a x = callback (during a) x

-- |'catch' equivalent.
catchE :: (EmbedIO m, Exception e) => m a -> (e -> m a) -> m a
catchE action handler = embed $ \x -> catch (action' x) (\e -> handler' e x)
    where
    action' x = callback action x
    handler' e x = callback (handler e) x

-- |'handle' equivalent.
handleE :: (EmbedIO m, Exception e) => (e -> m a) -> m a -> m a
handleE = flip catchE

-- |'try' equivalent.
tryE :: (EmbedIO m, Exception e) => m a -> m (Either e a)
tryE action = embed $ \x -> try (callback action x)

-- |'throw' equivalent.
throwE :: (EmbedIO m, Exception e) => e -> m a
throwE = liftIO . throwIO

-- |'forkIO' equivalent.
forkE :: EmbedIO m => m () -> m ThreadId
forkE action = embed $ \x -> forkIO (callback action x)
