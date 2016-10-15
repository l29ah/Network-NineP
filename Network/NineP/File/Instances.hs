-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- Instances for dealing with the usual data.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings, ViewPatterns #-}

module Network.NineP.File.Instances
    ( Convertible
    , ReadRef
    , WriteRef
    ) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Exception
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.Convertible.Base
import Data.Convertible.Instances
import Data.StateRef
import Data.Typeable
import Network.NineP.File

-- How do I avoid writing that?
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

-- | @'read'@ in @'Maybe'@.
safeRead :: (Read a) => String -> Maybe a
safeRead (reads -> [(v,"")]) = Just v
safeRead _ = Nothing

instance Convertible ByteString ByteString where
    safeConvert = Right . id

instance Convertible () ByteString where
    safeConvert x = convError "impossible to read that" x
instance Convertible ByteString () where
    safeConvert _ = Right ()

instance Convertible ByteString Bool where
    safeConvert s
        | l == "1" = Right True
        | l == "true" = Right True
        | l == "0" = Right False
        | l == "false" = Right False
        | otherwise = convError "doesn't look like a boolean value" l
            where l = trim $ B.unpack s
instance Convertible Bool ByteString where
    safeConvert True = Right "true"
    safeConvert False = Right "false"

instance (Show a, Num a) => Convertible a ByteString where
    safeConvert = Right . B.pack . show 
instance (Read a, Num a, Typeable a) => Convertible ByteString a where
    safeConvert s = maybe (convError "doesn't look like an integral value" l) id $ liftM Right =<< safeRead l
            where l = trim $ B.unpack s

-- TODO sane error
instance ReadRef () m ByteString where
    readReference = throw $ Underflow
instance Monad m => WriteRef () m ByteString where
    writeReference _ = return $ return ()

instance MonadIO m => ReadRef (Chan a) m a where
    readReference = liftIO . readChan
instance MonadIO m => WriteRef (Chan a) m a where
    writeReference r = liftIO . writeChan r
