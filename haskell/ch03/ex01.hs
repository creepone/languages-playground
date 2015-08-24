data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList :: List a -> [a]
toList Nil = []
toList (Cons a b) = a : (toList b)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + (max (treeHeight left) (treeHeight right))

ll :: [a] -> Int
ll [] = 0
ll (x:xs) = 1 + (ll xs)


data Moments = Moments {
    mCounts :: Int,
    mSum :: Double
} 
 deriving (Show)

lMoments [] = Moments 0 0
lMoments (x:xs) = let Moments c s = lMoments xs in Moments (c + 1) (s + x)

lAverage xs = let Moments c s = lMoments xs 
              in case c of
                 0 -> 0
                 _ -> s / fromIntegral c


pali [] = []
pali (x:xs) = x:(pali xs) ++ [x]

rev [] = []
rev (x:xs) = (rev xs) ++ [x]

isPali xs = xs == (rev xs)

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse sep (x:xs) = x ++ [sep] ++ (intersperse sep xs)