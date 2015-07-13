
-- | Binary Search tree
data BT a = Nil | Node a (BT a) (BT a)



-- | Structure to hold a Prime Factorization
data PrFact a = Prime a | Node a (PrFact a) (PrFact a)

flatten :: PrFact a -> [a]
flatten (Prime p) = [p]
flatten (Node c t1 t2) = flatten t1 ++ flatten t2


