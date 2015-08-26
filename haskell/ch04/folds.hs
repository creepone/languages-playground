import Data.Char (digitToInt)

asInt :: String -> Int
asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
                  
                  
asIntFold :: String -> Int
asIntFold ('-':xs) = -(asIntFold xs)
asIntFold xs = foldl step 0 xs
  where step s x = 10 * s + digitToInt x