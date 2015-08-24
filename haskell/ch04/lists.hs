import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = let Just rest = safeInit xs in Just (x:rest)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith predicate input =
    accumulate [] input 
    where accumulate acc [] = 
            if null acc then [] else [reverse acc]
          accumulate acc (x:xs)
            | predicate x = accumulate (x:acc) xs
            | otherwise = if null acc 
                          then accumulate [] xs
                          else (reverse acc) : (accumulate [] xs)   
                          
(|>) x f = f x
                         
pickFirst [] = []
pickFirst ([]:xs) = pickFirst xs
pickFirst ((x:xs):ys) = x:(pickFirst ys)

dropFirst [] = []
dropFirst ([]:xs) = []:(dropFirst xs)
dropFirst ((x:xs):ys) = xs:(dropFirst ys)
    
isOnlyEmpty [] = True
isOnlyEmpty ([]:xs) = isOnlyEmpty xs
isOnlyEmpty ((x:xs):ys) = False

transLines acc lines =
    let w = lines |> pickFirst
        rest = lines |> dropFirst
    in if (isOnlyEmpty rest) 
       then (w:acc) |> reverse
       else transLines (w:acc) rest
       
transText text =
    text |> lines |> transLines [] |> unlines