-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die

module Network.NineP.Error
	( NineError(..)
	, module Control.Monad.Error
	) where

import Control.Monad.Error
import Data.Word

-- |Throwable errors
data NineError = 
	ENotImplemented String |
	ENotADir |
	EDir |
	ENoFile String |
	ENoFid Word32 |
	ENoAuthRequired |
	EPermissionDenied |
	OtherError String

instance Error NineError where
	noMsg = undefined
	strMsg = OtherError

-- |See also: @linux/net/9p/error.c@
instance Show NineError where
	show (ENotImplemented s) = s ++ " is not implemented"
	show ENotADir = "not a directory"
	show EDir = "Is a directory"
	show (ENoFile s) = "file not found"
	show (ENoFid i) = "fid " ++ show i ++ " is not registered on the server"
	show ENoAuthRequired = "the server doesn't require any kind of authentication"
	show EPermissionDenied = "permission denied"
	show (OtherError s) = s
