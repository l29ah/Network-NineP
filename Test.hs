{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Error
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Word

import Network.NineP
import Network.NineP.Error
import Network.NineP.File

--cfg = Config $ boringDir "/" [("boring", boringFile "boring"),("nyak", boringFile "nyak")]
spamwr :: Word64 -> B.ByteString -> ErrorT NineError IO (Word32)
spamwr _ d = do
	lift $ B.putStr d
	return $ fromIntegral $ B.length d

boringwr _ c = return $ B.take (fromIntegral c) "i am so very boring"

cfg = Config {
	root = boringDir "/" [("lol", (boringFile "lol") { write = spamwr })],
	addr = "unix!/tmp/h9pt"
}

main = run9PServer cfg
