module Error
	( NineError(..)
	, module Control.Monad.Error
	) where

import Control.Monad.Error

data NineError = 
	ENotADir |
	ENoFile String |
	OtherError String

instance Error NineError where
	noMsg = undefined
	strMsg = OtherError

instance Show NineError where
	show ENotADir = "tried to walk into a non-directory"
	show (ENoFile s) = "file " ++ s ++ " not found"
	show (OtherError s) = s
