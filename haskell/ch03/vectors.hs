data Direction = Left | Right | Straight
    deriving (Show)

data Point = Point Double Double 
    deriving (Show)

turn (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | z > 0 = Main.Left
    | z < 0 = Main.Right
    | otherwise = Main.Straight
        where (a1, b1) = (x2 - x1, y2 - y1)
              (a2, b2) = (x3 - x2, y3 - y2)
              z = a1 * b2 - a2 * b1


turns (a:b:c:xs) = (turn a b c):(turns (b:c:xs))
turns _ = []