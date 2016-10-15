{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Word
import System.Log.Logger

import Network.NineP as N
import Network.NineP.Error
import Network.NineP.File

--cfg = Config $ boringDir "/" [("boring", boringFile "boring"),("nyak", boringFile "nyak")]
spamwr :: Word64 -> B.ByteString -> IO Word32
spamwr _ d = do
        B.putStr d
        return $ fromIntegral $ B.length d

boringwr _ c = return $ B.take (fromIntegral c) "i am so very boring"

main = do
        updateGlobalLogger "Network.NineP" $ setLevel DEBUG
        memd <- memoryDirectory "/" :: IO (NineFile IO)
        run9PServer $ Config {
                        -- TODO Data.Default?
                        root = boringDir "/" [
                                ("lol", (boringFile "lol") { write = spamwr }),
                                ("memd", memd),
                                ("exception", (boringFile "exception") { N.read = \_ _ -> throw $ AssertionFailed "lol" })],
                        addr = "unix!/tmp/h9pt",
                        monadState = undefined
                }
