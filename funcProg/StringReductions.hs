
reduce :: String -> String
reduce [] = []
reduce (c:cs) = c : ( reduce $ filter (\x -> x /= c)  cs)

main :: IO ()
main = do
    string <- getLine
    let 
       out = reduce string
    putStrLn out

