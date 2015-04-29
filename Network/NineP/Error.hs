-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die

module Network.NineP.Error
	( NineError(..)
	) where

import Control.Exception
import Data.Typeable
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
	EInval |
	OtherError String deriving (Typeable)

instance Exception NineError

-- |See also: @linux/net/9p/error.c@
instance Show NineError where
	show (ENotImplemented s) = s ++ " is not implemented"
	show ENotADir = "not a directory"
	show EDir = "Is a directory"
	show (ENoFile s) = "file not found"
	show (ENoFid i) = "fid " ++ show i ++ " is not registered on the server"
	show ENoAuthRequired = "the server doesn't require any kind of authentication"
	show EPermissionDenied = "permission denied"
	show EInval = "Invalid argument"
	show (OtherError s) = s
