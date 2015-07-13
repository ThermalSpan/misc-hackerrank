
import Control.Monad

subsetSum :: Int -> Int -> Int -> [Int] -> Int
subsetSum sum size goal [] = if sum >= goal then size else -1
subsetSum sum size goal (x:xs) = case (option1 > 0, option2 > 0) of
      (True, True) -> min option1 option2
      (True, False) -> option1
      (False, True) -> option2
      (False, False) -> -1
  where
      option1 = subsetSum (sum + x) (size + 1) goal xs
      option2 = subsetSum sum size goal xs


main :: IO ()
main = do
    listSize <- getLine
    list     <- getLine
    testSize <- getLine
    tests    <- replicateM (read testSize) getLine
    let
        listI = map (read :: String -> Int) $ words list
        testI = map (read :: String -> Int) tests
        sol = map (\g -> subsetSum 0 0 g listI) testI
    mapM_ print sol
