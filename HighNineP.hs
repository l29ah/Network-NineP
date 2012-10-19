module HighNineP where

import qualified Data.ByteString as B
import FidMap
import Data.Word

data NineFile = NineFile {
        nineRead :: Word32 -> Word32 -> IO (B.ByteString),
        nineWrite :: Word64 -> B.ByteString -> IO (Word32)
}

data NineClient = NineClient {
        fidMap :: FidMap NineFile
}
