module FidMap (
     emptyFidMap
    ,insertFid
    ,findByFid
    ,FidMap
    ) where

import Data.Tree.RBTree

newtype Pair a b = Pair {fromPair :: (a, b)}

instance (Eq a) => Eq (Pair a b) where
    Pair (a1, _) == Pair (a2, _) = a1 == a2

instance (Ord a) => Ord (Pair a b) where
    Pair (a1, _) <= Pair (a2, _) = a1<=a2


newtype FidMap a = FidMap (RBTree (Pair Int a))

emptyFidMap :: FidMap a
emptyFidMap = FidMap emptyRB

insertFid :: FidMap a -> Int -> a -> FidMap a
insertFid (FidMap m) fid v = FidMap $ insertOrd m $ Pair (fid, v)

--deleteFid :: FidMap a -> 


findByFid :: FidMap a -> Int -> Maybe a
findByFid (FidMap m) fid = fmap (snd.fromPair) $ searchFast cmp m fid
    where   cmp :: Int -> Pair Int a -> Ordering
            cmp fid1 (Pair (fid2, _)) = compare fid1 fid2


