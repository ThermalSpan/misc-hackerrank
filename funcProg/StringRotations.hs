import Control.Monad

stringStream :: String -> String
stringStream s = s ++  stringStream s

getRotation :: String -> Int -> Int -> String
getRotation s len i = take len $ drop i (stringStream s)

rotations :: String -> [String]
rotations s = map (getRotation s len) [1..len]
  where
      len = length s

printSol :: [String] -> IO ()
printSol xs = do
    mapM_ (\s -> putStr (s ++ " ")) xs
    putStrLn ""

main :: IO ()
main = do
    input <- getLine
    strings <- replicateM (read input) getLine
    let
        sol = map rotations strings
    mapM_ printSol sol
