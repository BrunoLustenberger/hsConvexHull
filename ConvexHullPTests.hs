{- |
Module : ConvexHullPTests
Description : Unit tests for module ConvexHull
Copyright : (c) 2021 Bruno M.S.  Lustenberger

You can run these tests from ghci by loading this module and then
calling the functions quickCheck or verboseCheck for these properties.
Examples:
-- quickCheck, 100 tests
*ConvexHullPTests> quickCheck propConvexHull
-- quickCheck, 1000 tests
*ConvexHullPTests> quickCheck $ withMaxSuccess 1000  propConvexHull
-- verboseCheck, 100 tests
*ConvexHullPTests> verboseCheck propConvexHull
-- verboseCheck, 1000 tests
*ConvexHullPTests> verboseCheck $ withMaxSuccess 1000  propConvexHull

-}
module ConvexHullPTests (
      propLeftLowestPoint
    , propIdempotent
    , propConvexHull
    ) where

import Data.List (sortBy, minimumBy)
import Data.Ratio
import Test.QuickCheck

import ConvexHull

-- generating point lists arbitrarily
-- todo: restrict x,y to a relevant range, e.g. -100..+100
instance Arbitrary (Pt) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Pt x y)

-- |left lowest point of a list of points
propLeftLowestPoint :: [Pt] -> Bool
propLeftLowestPoint [] = True
propLeftLowestPoint ps = leftLowestPoint ps == minimumBy cmpyx ps
    where cmpyx (Pt x1 y1) (Pt x2 y2)
            | y1 < y2               = LT
            | y1 == y2 && x1 < x2   = LT
            | y1 == y2 && x1 == x2  = EQ
            | otherwise             = GT

-- |Idempotent: applying convexHull a 2nd time changes nothing
propIdempotent :: [Pt] -> Bool
propIdempotent ps = convexHull (convexHull ps) == convexHull ps

-- |All points are on the left of the directed closed path given by the hull
--  or on this path itself.
propConvexHull :: [Pt] -> Bool
propConvexHull ps = onLeftOfOrOnSegments ps ss
    where ss   = myZip ch ch'
          ch   = convexHull ps
          ch'  = if ch == [] then [] else tail ch ++ [head ch] 
          -- ch' is rotate left of ch
          -- ss is a list of pairs of points, 
          -- each pair corresonds to 1 segement of the convex hull
          -- including the segment from last point to first point 

-- Takes a list of points and a list of segments. Returns True iff
-- for all segments onLeftOfOrOnSegment (see below) holds
onLeftOfOrOnSegments :: [Pt] -> [(Pt,Pt)] -> Bool
onLeftOfOrOnSegments ps (s:segs)  = onLeftOfOrOnSegment ps s && 
                                    onLeftOfOrOnSegments ps segs
onLeftOfOrOnSegments _ []         = True

-- True iff all points in [Pt] are to the left of the segment 
-- or equal to start or end of the segment
onLeftOfOrOnSegment :: [Pt] -> (Pt,Pt) -> Bool
onLeftOfOrOnSegment [] _            = True
onLeftOfOrOnSegment (p:ps) (p1,p2)  = 
    (p == p1 || p == p2 || directionOf p1 p2 p /= DirRight) && 
    onLeftOfOrOnSegment ps (p1,p2)

-- zip 2 lists into a list of pairs
myZip :: [a] -> [b] -> [(a,b)]
myZip (x:xs) (y:ys)   = (x,y) : myZip xs ys
myZip [] _ = []
myZip _ [] = []
              