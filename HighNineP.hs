import qualified Data.ByteString as B
import FidMap
import Data.Int

data NineFile = NineFile {
        nineRead :: Int -> Int -> IO (B.ByteString),
        nineWrite :: Int64 -> B.ByteString -> IO (Int)
}

data NineClient = NineClient {
        fidMap :: FidMap NineFile
}
