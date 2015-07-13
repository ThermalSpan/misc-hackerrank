import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

data Heap k v = Nil | Node (k, v) (Heap k v) (Heap k v)
type State = (Int, Heap Int Int, Int, Heap Int Int)

merge :: Ord k => Heap k v -> Heap k v -> Heap k v
merge t1 Nil = t1
merge Nil t2 = t2
merge (Node (k1, v1) lt1 rt1) (Node (k2, v2) lt2 rt2) = case compare k1 k2 of
                                          LT -> Node (k1, v1) (merge rt1 (Node (k2, v2) lt2 rt2)) lt1
                                          EQ -> Node (k1, v1) (merge rt1 (Node (k2, v2) lt2 rt2)) lt1
                                          GT -> Node (k2, v2) (merge rt2 (Node (k1, v1) lt1 rt1)) lt2

insert :: Ord k => (k, v) -> Heap k v -> Heap k v
insert (k, v) = merge (Node (k, v) Nil Nil)

minValue :: Heap Int Int -> Int
minValue Nil = -1
minValue (Node (_,v) _ _) = v

deleteMin :: Ord k => Heap k v -> Heap k v
deleteMin Nil = Nil
deleteMin (Node _ tl tr) = tl `merge` tr

getMedian :: State -> Int
getMedian (minC, minH, maxC, maxH) = case compare minC maxC of
                                       LT -> minValue maxH
                                       GT -> minValue minH
                                       EQ -> minValue minH

balanceState :: State -> State
balanceState (minC, minH, maxC, maxH) =
    case (compare minC maxC, abs dif) of
      (_, 1)  -> (minC, minH, maxC, maxH)
      (EQ, _) -> (minC, minH, maxC, maxH)
      (LT, _) -> balanceState (minC + 1, insert  (-max, max ) minH, maxC - 1, deleteMin maxH)
      (GT, _) -> balanceState (minC - 1, deleteMin minH, maxC + 1, insert (min, min) maxH)
    where
        dif = maxC - minC
        min = minValue minH
        max = minValue maxH

nextState :: Int -> State -> State
nextState i (minC, minH, maxC, maxH) = balanceState $
    if i <= (minValue minH)
       then (minC + 1, insert (-i, i) minH, maxC, maxH)
       else (minC, minH, maxC + 1, insert (i, i) maxH)

iterateState :: Map.Map Int State -> Int -> State -> [Int] -> [Int]
iterateState _ _ _ [] = []
iterateState map cs state (x:xs) = case x >= 0 of
                               True -> getMedian stateI : iterateState (Map.insert cs stateI map) (cs + 1) stateI xs
                               False -> getMedian stateD : iterateState (Map.insert cs stateD map) (cs + 1) stateD xs
                             where
                                 stateI = nextState x state
                                 stateD = case Map.lookup (cs + x) map of
                                           Just s -> s
                                           otherwise -> (0, Nil, 0, Nil) 

main :: IO ()
main = do
    input <- getLine
    stream <- replicateM (read input) getLine
    let
       ints = map read $ concat $ map words stream
       outs = iterateState (Map.insert 0 (0,Nil,0,Nil) Map.empty) 1 (0,Nil,0,Nil) ints
    mapM_ print outs
