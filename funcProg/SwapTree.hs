import Text.Printf
import Control.Monad

data BT a = Nil | Node a (BT a) (BT a)

instance Show a => Show (BT a) where
    show Nil            = "Nil"
    show (Node x t1 t2) = "Node " ++ show x ++ "(" ++ show t1 ++ ", " ++ show t2 ++ ") "


flatten                 :: BT a -> [a]
flatten Nil             = []
flatten (Node a t1 t2)  = flatten t1 ++ [a] ++ flatten t2

swap                    :: Int -> Int -> BT a -> BT a
swap _ _ Nil              = Nil
swap o 1 (Node a t1 t2)   = Node a (swap o o t2) (swap o o t1)
swap o k (Node a t1 t2)   = Node a (swap o (k - 1) t1) (swap o (k - 1) t2)

getLvlSize :: [Int] -> Int
getLvlSize [] = 0
getLvlSize (x:xs) = case x of
        -1 -> getLvlSize xs
        otherwise -> 2 + getLvlSize xs

zipS :: [Int] -> [BT Int] -> [BT Int]
zipS [] _ = []
zipS (-1 : xs) ts =  Nil : zipS xs ts
zipS (x : xs) (t1 : t2 : ts) = (Node x t1 t2) : zipS xs ts 
zipS _ ts = [(Node 30 Nil Nil)] --Shouldn't get here EVER

splitLvls :: Int -> [Int] -> [[Int]]
splitLvls _ [] =[]
splitLvls n xs = curLvl : splitLvls nextSize remainder
  where
      curLvl = take n xs
      remainder = drop n xs
      nextSize = getLvlSize curLvl

buildTree :: [Int] -> BT Int
buildTree xs = tree
  where
      lvls = reverse $ splitLvls 1 xs
      ts = zipWith zipS lvls ([] : ts)
      tree = head $ last ts

printSol :: [Int] -> IO ()
printSol sl = do
    mapM_ (printf "%d ") sl
    printf "%s" "\n"

main :: IO ()
main = do
   input1 <- getLine
   inputs1 <- replicateM (read input1) getLine
   input2 <- getLine
   inputs2 <- replicateM (read input2) getLine
   let
    list  = map (read :: String -> Int) $ concat $ map words inputs1
    swaps = map (read :: String -> Int) $ concat $ map words inputs2
    lvls  = reverse $ splitLvls 1 (1 : list)
    build = zipWith zipS lvls ([] : build)
    t0    = head $ last build
    outt  = zipWith (\x y -> swap x x y) swaps (t0 : outt)
    outl  = map flatten outt 
   mapM_ printSol outl


