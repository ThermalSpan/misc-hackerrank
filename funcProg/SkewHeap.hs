{-
    SkewHeap.hs
    by Russell Bentley
    
    A Haskell Implementation of a skew heap.
-}
module SkewHeap (Heap, emptyHeap, merge, insert, delete, minKey, minKeyValue, deleteMin ) where

-- | A binary tree data type.
data Heap k v = Nil | Node (k, v) (Heap k v) (Heap k v)

-- | Blank
emptyHeap :: Heap k v
emptyHeap = Nil

            
-- | The 'merge' function merges two scew heaps.
merge :: Ord k => Heap k v -> Heap k v -> Heap k v
merge t1 Nil = t1
merge Nil t2 = t2
merge (Node (k1, v1) lt1 rt1) (Node (k2, v2) lt2 rt2) = case compare k1 k2 of 
                                          LT -> Node (k1, v1) (merge rt1 (Node (k2, v2) lt2 rt2)) lt1
                                          EQ -> Node (k1, v1) (merge rt1 (Node (k2, v2) lt2 rt2)) lt1
                                          GT -> Node (k2, v2) (merge rt2 (Node (k1, v1) lt1 rt1)) lt2

-- | The insert method is used to put an element in the heap.
insert :: Ord k => (k, v) -> Heap k v -> Heap k v
insert (k, v) = merge (Node (k, v) Nil Nil)

-- | The `delete` method removes an element from a heap.                         
delete                 :: Ord k => k -> Heap k v -> Heap k v
delete _ Nil            = Nil
delete k1 (Node (k2, v) tl tr) = case compare k1 k2 of 
                                LT -> Node (k2, v) (delete k1 tl) (delete k1 tr)
                                GT -> Node (k2, v) tl tr
                                EQ -> tl `merge` tr          

-- | NOTE! pattern mathing is non-exhaustive 
minKey :: Heap k v -> k
minKey (Node (k, _) _ _) = k

-- | NOTE! pattern mathing is non-exhaustive 
minKeyValue :: Heap k v -> (k, v)
minKeyValue (Node (k,v) _ _) = (k, v)

-- | Consider adding a deleteMinandInsert
deleteMin :: Ord k => Heap k v -> Heap k v
deleteMin Nil = Nil
deleteMin (Node _ tl tr) = tl `merge` tr




                                       
