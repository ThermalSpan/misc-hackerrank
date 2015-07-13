import Data.Char

sumIntDig :: String -> Int
sumIntDig xs = sum $ map digitToInt xs

superDigit :: String -> String
superDigit s = case length result of
                 1 -> result
                 otherwise -> superDigit result
                where
                    result = show $ sumIntDig s

main :: IO ()
main = do
    input <- getLine
    let
       nums = words input
       sd1 = sumIntDig (read $ nums !! 0)
       out = superDigits $ show $ (nums !! 1) * sd1
    putStrLn out

