module FidMap (
     emptyFidMap
    ,insertFid
    ,findByFid
    ,FidMap
    ) where

import Data.Tree.RBTree
import Data.Word

newtype Pair a b = Pair {fromPair :: (a, b)}

instance (Eq a) => Eq (Pair a b) where
    Pair (a1, _) == Pair (a2, _) = a1 == a2

instance (Ord a) => Ord (Pair a b) where
    Pair (a1, _) <= Pair (a2, _) = a1<=a2


newtype FidMap a = FidMap (RBTree (Pair Word32 a))

emptyFidMap :: FidMap a
emptyFidMap = FidMap emptyRB

insertFid :: FidMap a -> Word32 -> a -> FidMap a
insertFid (FidMap m) fid v = FidMap $ insertOrd m $ Pair (fid, v)

--deleteFid :: FidMap a -> 


findByFid :: FidMap a -> Word32 -> Maybe a
findByFid (FidMap m) fid = fmap (snd.fromPair) $ searchFast cmp m fid
    where   cmp fid1 (Pair (fid2, _)) = compare fid1 fid2


