module Error
	( NineError(..)
	, module Control.Monad.Error
	) where

import Control.Monad.Error
import Data.Word

data NineError = 
	ENotADir |
	ENoFile String |
	ENoFid Word32 |
	ENoAuthRequired |
	OtherError String

instance Error NineError where
	noMsg = undefined
	strMsg = OtherError

instance Show NineError where
	show ENotADir = "tried to walk into a non-directory"
	show (ENoFile s) = "file " ++ s ++ " not found"
	show (ENoFid i) = "fid " ++ show i ++ " is not registered on the server"
	show ENoAuthRequired = "the server doesn't require any kind of authentication"
	show (OtherError s) = s
