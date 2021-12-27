import Text.Printf
import Data.List (sortBy, minimumBy)
import Data.Ratio   -- to avoid comparison of doubles
import Test.QuickCheck

-- solve :: [(Int, Int)] -> Double
solve :: [(Int, Int)] -> Bool
solve points = -- Complete this function    
    -- lengthOfClosedPath ( convexHull (map getPtFromIntPair points) )
    prop_convexHull ( convexHull (map getPtFromIntPair points) )

main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let  
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  print ans

{- ============= -}

{- 0. Helper definitions from trigonometry -}

data Pt = Pt Int Int deriving (Show, Eq)
data Vr = Vr Int Int deriving (Show)

vrFromTo :: Pt -> Pt -> Vr
vrFromTo (Pt x1 y1) (Pt x2 y2) = Vr (x2-x1) (y2-y1)

crossp2d :: Vr -> Vr -> Int
crossp2d (Vr x1 y1) (Vr x2 y2) = x1*y2 - y1*x2

normSqr :: Vr -> Int
normSqr (Vr x y) = x^2 + y^2

{- 1. Step 1 of Graham Scan -}

leftLowestPoint :: [Pt] -> Pt
leftLowestPoint []     = error "at least 1 point needed"
leftLowestPoint [p]    = p
leftLowestPoint (p:ps) = 
    if (py < qy) || ((py == qy) && (px < qx)) then p else q
        where q = leftLowestPoint ps
              Pt qx qy = q
              Pt px py = p

{- 2. Step 2 of Graham Scan -}

pseudoAngleWithX :: Vr -> Ratio Int
pseudoAngleWithX (Vr 0 0) = 1  -- !
pseudoAngleWithX (Vr x y) = (signum x * x^2) % (x^2 + y^2)

compareVectors :: Vr -> Vr -> Ordering
compareVectors a b 
    | pseudoAngleWithX a > pseudoAngleWithX b   = LT
    | pseudoAngleWithX a < pseudoAngleWithX b   = GT
    | normSqr a < normSqr b                     = LT
    | normSqr a > normSqr b                     = GT
    | otherwise                                 = EQ

sortPoints :: Pt -> [Pt] -> [Pt]
sortPoints base ps = sortBy comparePoints ps
    where comparePoints p1 p2 = compareVectors (vrFromTo base p1) (vrFromTo base p2)

{- 3. Step 3 of Graham Scan -}

data Direction = DirLeft | DirStraight | DirRight deriving (Show, Eq)

directionOf :: Pt -> Pt -> Pt -> Direction
directionOf a b c 
    | test < 0   = DirRight
    | test > 0   = DirLeft
    | otherwise    = DirStraight
    where test = crossp2d v1 v2
          v2 = vrFromTo b c 
          v1 = vrFromTo a b

extendHull :: [Pt] -> Pt -> [Pt]
extendHull [] p   = [p]
extendHull [x] p  = if x /= p then [x,p] else [x]
extendHull hull p =
    if directionOf l1 l p == DirLeft then hull ++ [p]
                                     else extendHull (init hull) p
    where l = last hull
          l1 = last (init hull)

convexHullOfSorted :: [Pt] -> [Pt]
convexHullOfSorted ps
    | length ps < 2   = ps
    | otherwise       = extendHull prevHull p
    where prevHull = convexHullOfSorted (init ps)
          p = last ps

convexHull :: [Pt] -> [Pt]
convexHull ps
    | length ps < 2   = ps
    | otherwise       = convexHullOfSorted (sortPoints base ps)
    where base = leftLowestPoint ps

{- 4. Helper definitions for hackerrank submission -}

getPtFromIntPair :: (Int,Int) -> Pt
getPtFromIntPair (x,y) = Pt x y

norm :: Vr -> Double
norm (Vr x y) = sqrt (fromIntegral (x^2 + y^2))

lengthOfPath :: [Pt] -> Double
lengthOfPath [] = 0
lengthOfPath [p] = 0
lengthOfPath (p:ps) = norm (vrFromTo p (head ps)) + lengthOfPath ps   

lengthOfClosedPath :: [Pt] -> Double
lengthOfClosedPath [] = 0
lengthOfClosedPath [p] = 0
lengthOfClosedPath ps = lengthOfPath ps + norm (vrFromTo (last ps) (head ps))

{- 5. Tests -}

{- 5.1 Property Tests with QuickCheck -}
-- execute these tests from ghci by loading this module and then enter one of the QuickCheck vars qcn,
-- e.g.  ghci> qc1

prop_leftLowestPoint :: [Pt] -> Bool
prop_leftLowestPoint [] = True
prop_leftLowestPoint ps = leftLowestPoint ps == minimumBy cmpyx ps
    where cmpyx (Pt x1 y1) (Pt x2 y2)
            | y1 < y2               = LT
            | y1 == y2 && x1 < x2   = LT
            | y1 == y2 && x1 == x2  = EQ
            | otherwise             = GT

prop_idempotent :: [Pt] -> Bool
prop_idempotent ps = convexHull (convexHull ps) == convexHull ps

prop_convexHull :: [Pt] -> Bool
-- all points are on the left of the directed closed path given by the hull
-- or on this path itself
prop_convexHull ps = onLeftOfOrOnSegments ps ss
    where ss   = myZip ch ch'
          ch   = convexHull ps
          ch'  = if ch == [] then [] else tail ch ++ [head ch] -- rotate left of ch

onLeftOfOrOnSegments :: [Pt] -> [(Pt,Pt)] -> Bool
onLeftOfOrOnSegments ps (s:segs)  = onLeftOfOrOnSegment ps s && onLeftOfOrOnSegments ps segs
onLeftOfOrOnSegments _ []         = True

-- True if all points in [Pt] are to the left of the segment or equal to start or end of the segment
onLeftOfOrOnSegment :: [Pt] -> (Pt,Pt) -> Bool
onLeftOfOrOnSegment [] _            = True
onLeftOfOrOnSegment (p:ps) (p1,p2)  = 
    (p == p1 || p == p2 || directionOf p1 p2 p /= DirRight) && onLeftOfOrOnSegment ps (p1,p2)
    
myZip :: [a] -> [b] -> [(a,b)]
myZip (x:xs) (y:ys)   = (x,y) : myZip xs ys
myZip [] _ = []
myZip _ [] = []
              
instance Arbitrary (Pt) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pt x y)

