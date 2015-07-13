import Control.Monad
import Data.MultiSet
import qualified Data.Map as Map

type State = (Int, MultiSet Int, Int, MultiSet Int)

getMedian (minC, minH, maxC, maxH) = case compare minC maxC of
                                       LT -> findMin maxH
                                       GT -> findMax minH
                                       EQ -> findMax minH

balanceState (minC, minH, maxC, maxH) =
    case (compare minC maxC, abs dif) of
      (_, 1)  -> (minC, minH, maxC, maxH) 
      (EQ, _) -> (minC, minH, maxC, maxH)
      (LT, _) -> balanceState (minC + 1, insert max minH, maxC - 1, deleteMin maxH)
      (GT, _) -> balanceState (minC - 1, deleteMin minH, maxC + 1, insert min maxH)
    where
        dif = maxC - minC
        min = findMax minH
        max = findMin maxH

nextState i (minC, minH, maxC, maxH) = balanceState $
    if i <= (findMin minH) 
       then (minC + 1, insert i minH, maxC, maxH)
       else (minC, minH, maxC + 1, insert i  maxH)

iterateState :: Map.Map Int State -> Int -> [Int] -> [Int]
iterateState _ _ [] = []
iterateState map cs (x:xs) = case x >= 0 of
                               True -> getMedian stateI : iterateState (Map.insert cs stateI map) xs 
                               False -> getMedian stateD : iterateState (Map.insert cs stateI) xs 
                             where
                                 stateI = nextState x s
                                 stateD = head $ drop ((abs x) - 1) (s:ss)

main :: IO ()
main = do 
    input <- getLine
    stream <- replicateM (read input) getLine
    let 
       ints = Prelude.map read $ concat $ Prelude.map words stream
       outs = iterateState [(0,empty,0,empty)] ints
    mapM_ print outs
