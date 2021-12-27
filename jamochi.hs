-- eigene ergänzungen 
import Data.List (sortOn)
type Coordinate = (Int, Int) -- (Double,Double)
data Direction = LeftAngle | RightAngle | StraightAngle deriving (Eq)

-- jamochi,
-- einige indents von mir angepasst
calcDirection :: Coordinate -> Coordinate -> Coordinate -> Direction
calcDirection (ax,ay) (bx,by) (cx,cy)
    | resultOfK < 0 = RightAngle
    | resultOfK > 0 = LeftAngle
    | otherwise = StraightAngle
    where resultOfK = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

-- takes two lists of coordinates, first is points, second is stack
-- FIXED BUG, forgot to sort list before applying algo
grahamScan :: [Coordinate] -> [Coordinate] -> [Coordinate]
grahamScan [] [] = []
grahamScan (p:[]) [] = p:[]
grahamScan p [] = grahamScan ps (p2:p1:[])
    where (p1:p2:ps) = grahamSort p
grahamScan (p:ps) (s:[]) = grahamScan ps (p:s:[])
grahamScan [] s = reverse s
grahamScan (p:ps) (s1:s2:ss)
    | d == RightAngle || d == StraightAngle = grahamScan ps (p:s1:s2:ss)
    | otherwise = grahamScan (p:ps) (s2:ss)
    where d = calcDirection s1 s2 p

grahamSort [] = []
grahamSort x = (xp0, yp0) : sortOn condition xs
    where ((xp0, yp0):xs) = sortOn snd (sortOn fst x)
          condition (x2,y2) = atan((fromIntegral (y2 - yp0)) / (fromIntegral (x2 - xp0)))

p00 =  (0, 0 )
p01 =  (0, 1 )
p02 =  (0, 2 )
p03 =  (0, 3 )
p10 =  (1, 0 )
p11 =  (1, 1 )
p12 =  (1, 2 )
p13 =  (1, 3 )
p20 =  (2, 0 )
p21 =  (2, 1 )
p22 =  (2, 2 )
p23 =  (2, 3 )
p30 =  (3, 0 )
p31 =  (3, 1 )
p32 =  (3, 2 )
p33 =  (3, 3 )


ch0 = grahamSort []
ch1 =  grahamSort [p10]
ch2 =  grahamSort [p20,p10]
ch3 =  grahamSort [p01,p00,p10]
ch4 =  grahamSort [p01,p00,p02,p11]
ch5 =  grahamSort [p00,p10,p20,p01,p02]
ch6 =  grahamSort [p00,p10,p20,p01,p12,p11]
ch7 =  grahamSort [p20,p21,p22,p00,p01,p02,p10,p11,p12]
ch7' =  grahamSort [p20,p21,p22,p00,p01,p02,p10,p11,p12,     p20,p00,p11,p21,p02]
ch8 =  grahamSort [p20,p21,p22,p00,p01,p02,p03,p10,p11,p12]
ch8' =  grahamSort [p20,p21,p22,p00,p01,p02,p03,p10,p11,p12,    p11,p03,p02,p12,p22]