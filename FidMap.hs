module FidMap (
     empty
    ,insert
    ,(!)
    ,FidMap
    ,fromList
    ) where

import Data.Tree.RBTree (RBTree)
import qualified Data.Tree.RBTree as RB
import Data.Word

newtype Pair a b = Pair {fromPair :: (a, b)}

instance (Eq a) => Eq (Pair a b) where
    Pair (a1, _) == Pair (a2, _) = a1 == a2

instance (Ord a) => Ord (Pair a b) where
    Pair (a1, _) <= Pair (a2, _) = a1<=a2


newtype FidMap a = FidMap (RBTree (Pair Word32 a))

empty :: FidMap a
empty = FidMap RB.emptyRB

insert :: Word32 -> a -> FidMap a -> FidMap a
insert fid v (FidMap m) = FidMap $ RB.insertOrd m $ Pair (fid, v)

--deleteFid :: FidMap a -> 


(!) :: FidMap a -> Word32 -> Maybe a
(!) (FidMap m) fid = fmap (snd.fromPair) $ RB.searchFast cmp m fid
    where   cmp fid1 (Pair (fid2, _)) = compare fid1 fid2

fromList :: [(Word32, a)] -> FidMap a
fromList l = FidMap $ RB.insertOrdList RB.emptyRB $ map Pair l
