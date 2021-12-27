import Data.List (sortBy)
-- general defs

data Point2D = Point2D {
                 xp :: Double,
                 yp :: Double
               } deriving (Show)

data Vector2D = Vector2D {
                  xv :: Double,
                  yv :: Double
                } deriving (Show)

fromPoints :: Point2D -> Point2D -> Vector2D
fromPoints (Point2D x1 y1) (Point2D x2 y2) = Vector2D (x2-x1) (y2-y1)

-- hint: instead of the following 2 functions, use the crossproduct of 2 vectors
-- vector turned to left by 90°
left90 :: Vector2D -> Vector2D
left90 (Vector2D x y) = Vector2D (-y) x

-- dotp of vectors
dotp :: Vector2D -> Vector2D -> Double
-- dotp (Vector2D x1 y1) (Vector2D x2 y2) = x1*y1 + x2*y2
dotp v1 v2 = xv v1 * xv v2 + yv v1 * yv v2 -- less readable!

-- tests
p0 = Point2D 0 0
p1 = Point2D 2 1
p1' = Point2D 4 2
v1 = fromPoints p1 p1'
p2 = Point2D 3 2
p3 = Point2D 3 1
v2 = left90 v1

-- direction of 3 points

data Direction = DirLeft | DirStraight | DirRight deriving (Show, Eq)

directionOf :: Point2D -> Point2D -> Point2D -> Direction
directionOf a b c 
    | test < 0.0   = DirRight
    | test > 0.0   = DirLeft
    | otherwise    = DirStraight
    where   test = dotp (left90 v1) v2
            v2 = fromPoints b c -- note: local var shadowing outer var
            v1 = fromPoints a b

directionsOf :: [Point2D] -> [Direction]
directionsOf ps        
    | length ps < 3   = error "at least 3 points needed"
    | length ps == 3  = [directionOf p1 p2 p3]
    | otherwise       = (directionOf p1 p2 p3) : (directionsOf (tail ps))
    where p1 = head ps
          p2 = head (tail ps)
          p3 = head (tail (tail ps))

-- tests
e0  = Point2D 0 0
e1  = Point2D 1 1
e2  = Point2D 3 1
e3  = Point2D 1 3
e4  = Point2D 3 4
e5  = Point2D 5 2
e6  = Point2D (-2) 4
e7  = Point2D (-1) 2
e8  = Point2D (-2) (-1)
e9  = Point2D (-3) 2
e10  = Point2D (-3) (-2)
e11  = Point2D 1 (-3)
e12  = Point2D 3 (-2)
e13  = Point2D 1 (-1)
e14  = Point2D 4 (-2)

dirs1 = directionsOf [e0,e1,e2] 
dirs2 = directionsOf [e0,e1,e2,e3,e4,e5,e6,e7,e8] 

-- step 1 of Graham Scan: find points with lowest y-coordinate 
-- and among them the one with lowest x-coordinate. If there are
-- more than one such point in the list (which are thus equal), take any of them.
-- We could sort the list using a lexicographic compare function first y then x
-- But here I try an alogrithm with linear time.

findLeftLowestPoint :: [Point2D] -> Point2D
findLeftLowestPoint []     = error "at least 1 point needed"
findLeftLowestPoint [p]    = p
findLeftLowestPoint (p:ps) = 
    if (py < qy) || ((py == qy) && (px < qx)) then p else q
        where q = findLeftLowestPoint ps
              py = yp p
              qy = yp q
              px = xp p
              qx = xp q 

set0 = []
set1 = [e1]
set2 = [e13,e12]
set2' = [e8,e2]
set2'' = [e13,e8]
set3 = [e2,e13,e8]
set4 = [e1,e6,e14]
set4' = [e1,e14,e14,e6,e14]

setAll = [e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14]

rset1 = findLeftLowestPoint set1
rset2 = findLeftLowestPoint set2
rset2' = findLeftLowestPoint set2'
rset2'' = findLeftLowestPoint set2''
rset3 = findLeftLowestPoint set3
rset4 = findLeftLowestPoint set4
rset4' = findLeftLowestPoint set4'
rsetAll= findLeftLowestPoint setAll

-- step2 of Graham Scan

norm :: Vector2D -> Double
norm (Vector2D x y) = sqrt (x^2 + y^2) 

-- angle of a vector relative to x-axis
-- instead of angle, its cosine suffices
-- note: a vector and the vector mirrored at the x-axis get the same value
cosAngleWithX :: Vector2D -> Double
cosAngleWithX (Vector2D x y)
    | n > 0      = x / n
    | otherwise  = 1 
        -- for our purpose the 0 vector must have a 0° (not 90°) angle with all others
    where n = norm (Vector2D x y)

a1 = cosAngleWithX (Vector2D 0 0)
a2 = cosAngleWithX (Vector2D 1 0)
a3 = cosAngleWithX (Vector2D 1 1)
a3' = cosAngleWithX (Vector2D 1 (-1))
a4 = cosAngleWithX (Vector2D 0 1)
a5 = cosAngleWithX (Vector2D (-1) 1)
a5' = cosAngleWithX (Vector2D (-1) (-1))


-- compare 2 vectors according to their angle with x-axis
compareVectors :: Vector2D -> Vector2D -> Ordering
compareVectors a b 
    | cosAngleWithX a > cosAngleWithX b   = LT
    | cosAngleWithX a < cosAngleWithX b   = GT
    | norm a < norm b                     = LT
    | norm a > norm b                     = GT
    | otherwise                           = EQ
    -- when 2 vectors have the same angle with x-Axis their norm is compared
    -- the final algorithm would not work correctly, if this were omitted.

b0 = compareVectors (fromPoints e0 e1) (fromPoints e0 e13)
b1 = compareVectors (fromPoints e11 e14) (fromPoints e11 e12)
b2 = compareVectors (fromPoints e1 e9) (fromPoints e1 e7)
b3 = compareVectors (fromPoints e2 e7) (fromPoints e2 e5)

-- sort points relative to origin and angle with x-axis
sortPoints :: Point2D -> [Point2D] -> [Point2D]
sortPoints origin ps = sortBy comparePoints ps
    where comparePoints p1 p2 = compareVectors (fromPoints origin p1) (fromPoints origin p2)

c0 = sortPoints e0 []
c1 = sortPoints e1 [e1]
c2 = sortPoints e1 [e4,e5]
cAll = sortPoints e11 setAll

-- step 3 of Graham Scan

-- assuming hull is a list containing no DirRight directions
-- extend this hull with a new point, so that there still is no DirRight
-- instead of n DirRight only DirLeft (or only <= points)
extendHull :: [Point2D] -> Point2D -> [Point2D]
extendHull [] p  = [p]
extendHull [x] p = [x,p]
extendHull hull p =
    {-
    if directionOf l1 l p == DirRight then extendHull (init hull) p
                                      else hull ++ [p]
    -}
    -- better: only left instead of no right.
    if directionOf l1 l p == DirLeft then hull ++ [p]
                                     else extendHull (init hull) p
    where l = last hull
          l1 = last (init hull)

d0 = extendHull [] e11
d1 = extendHull [e1] e8
d2 = extendHull [e1,e5] e4
d2' = extendHull [e1,e5] e14
d3 = extendHull [e0,e13,e14] e2
d3' = extendHull [e0,e13,e2] e5
d4 = extendHull [e0,e13,e2,e3] e5

-- helper function: assumes points are already sorted
hullOfSorted :: [Point2D] -> [Point2D]
hullOfSorted ps
    {-
    | length ps < 3   = error "at least 3 points needed"
    | length ps == 3  = ps
    -}
    | length ps < 2   = error "at least 2 points needed"
    | length ps == 2  = ps
    | otherwise       = extendHull prevHull p
    where prevHull = hullOfSorted (init ps)
          p = last ps

f1 = hullOfSorted [e0,e2,e5,e1,e4,e3,e7,e6,e9]

convexHull :: [Point2D] -> [Point2D]
convexHull ps
    | length ps < 3   = ps
    | otherwise       = hullOfSorted (sortPoints origin ps)
    where origin = findLeftLowestPoint ps

p00 = Point2D 0 0
p01 = Point2D 0 1
p02 = Point2D 0 2
p03 = Point2D 0 3
p10 = Point2D 1 0
p11 = Point2D 1 1
p12 = Point2D 1 2
p13 = Point2D 1 3
p20 = Point2D 2 0
p21 = Point2D 2 1
p22 = Point2D 2 2
p23 = Point2D 2 3
p30 = Point2D 3 0
p31 = Point2D 3 1
p32 = Point2D 3 2
p33 = Point2D 3 3


ch0 = convexHull []
ch1 = convexHull [p10]
ch2 = convexHull [p20,p10]
ch3 = convexHull [p01,p00,p10]
ch4 = convexHull [p01,p00,p02,p11]
ch5 = convexHull [p00,p10,p20,p01,p02]
ch6 = convexHull [p00,p10,p20,p01,p12,p11]
ch7 = convexHull [p20,p21,p22,p00,p01,p02,p10,p11,p12]
ch7' = convexHull [p20,p21,p22,p00,p01,p02,p10,p11,p12,     p20,p00,p11,p21,p02]
ch8 = convexHull [p20,p21,p22,p00,p01,p02,p03,p10,p11,p12]
ch8' = convexHull [p20,p21,p22,p00,p01,p02,p03,p10,p11,p12,    p11,p03,p02,p12,p22]

chAll = convexHull setAll

bashford = convexHull [
  Point2D 2.282154 1.107697 , -- origin at beginning!
  Point2D 0.508136 3.557593 ,
  Point2D 6.490489 7.220862 ,
  Point2D 5.965358 7.999035 ,
  Point2D 8.800029 5.776057 ,
  Point2D 8.404778 6.139152 ,
  Point2D 0.743481 2.498024 ,
  Point2D 1.002515 1.441651 ,
  Point2D 6.814331 4.42529 ,
  Point2D 0.351636 7.519613 ,
  Point2D 7.209313 4.625614 ,
  Point2D 3.246308 8.286591 ,
  Point2D 1.427197 8.78825 ,
  Point2D 8.295495 1.439363 ,
  Point2D 6.41094 2.408661 ,
  Point2D 4.719379 4.218519 ,
  Point2D 5.873703 6.961398 ,
  Point2D 6.572547 5.621853 ,
  Point2D 1.308362 2.677804 ,
  Point2D 3.789837 3.064397 
  ]
{-
result
2.282154 1.107697
8.295495 1.439363
8.800029 5.776057
8.404778 6.139152
5.965358 7.999035
1.427197 8.78825
0.351636 7.519613
0.508136 3.557593
0.743481 2.498024
1.002515 1.441651
-}

cw2014 = convexHull[ 
      Point2D 6 2
    , Point2D 2 2
    , Point2D 3 3
    , Point2D 0 0
    , Point2D 7 7
    , Point2D 5 10
    , Point2D (-3) 2
    , Point2D 5 1
    , Point2D 1 1
    , Point2D 2 6
  ]

reimai = convexHull [
    p20, p11, p12, p23,
    p00, p01, p30, p31,
    p02, p03, p32, p33,
    p10, p21, p22, p13
    ]

-- hackerrank

hr00 = convexHull [
       Point2D 1 1
     , Point2D 2 5
     , Point2D 3 3
     , Point2D 5 3
     , Point2D 3 2
     , Point2D 2 2
    ]
-- perimeter = 8.3

hr01 = convexHull [
    Point2D 3 2,
    Point2D 2 5,
    Point2D 4 5
    ]
-- perimeter = 12.2

-- perimeter for hr

lengthOfPath :: [Point2D] -> Double
lengthOfPath [] = 0
lengthOfPath [p] = 0
lengthOfPath (p:ps) = norm (fromPoints p q) + lengthOfPath ps   
                      where q = head ps

lengthOfClosedPath :: [Point2D] -> Double
lengthOfClosedPath [] = 0
lengthOfClosedPath [p] = 0
lengthOfClosedPath ps = lengthOfPath ps + norm (fromPoints (last ps) (head ps))

hr00res = lengthOfClosedPath hr00
hr01res = lengthOfClosedPath hr01

