{- |
Module : ConvexHull
Description : Compute the convex hull of a finite set of points in a plane.
Copyright : (c) 2021 Bruno M.S. Lustenberger
Exercise 12 of Chapter 3 of http://book.realworldhaskell.org/ 
The "Graham Scan" algorithm is used. 
See https://en.wikipedia.org/wiki/Graham_scan
-}
{-
module ConvexHull (
      Pt(..)
    , Vr(..)
    , convexHull
    ) where
-}

module ConvexHull where

import Data.List (sortBy, minimumBy)
import Data.Ratio   -- to avoid comparison of doubles
import Test.QuickCheck
import Test.HUnit

{- 0. Helper definitions from trigonometry -}

-- Point in the x,y-plane
data Pt = Pt Int Int deriving (Show, Eq)

-- Vector in the x,y-plane
data Vr = Vr Int Int deriving (Show)

-- Vector from point p1 to point p2
vrFromTo :: Pt -> Pt -> Vr
vrFromTo (Pt x1 y1) (Pt x2 y2) = Vr (x2-x1) (y2-y1)

-- crossproduct of two 2-dimensional vectors: 
-- the 3rd component of the crossproduct of the corresponding 3-dimensional vectors
crossp2d :: Vr -> Vr -> Int
crossp2d (Vr x1 y1) (Vr x2 y2) = x1*y2 - y1*x2

-- square of length of a vector
normSqr :: Vr -> Int
normSqr (Vr x y) = x^2 + y^2

{- 1. Step 1 of Graham Scan: find points with lowest y-coordinate 
   and among them the one with lowest x-coordinate. If there are
   more than one such point in the list (which are thus equal), 
   take any of them.
-}
-- I could sort the list using a lexicographic order comparing first y then x,
-- but here I try an alogrithm with linear time.

leftLowestPoint :: [Pt] -> Pt
leftLowestPoint []     = error "at least 1 point needed"
leftLowestPoint [p]    = p
leftLowestPoint (p:ps) = 
    if (py < qy) || ((py == qy) && (px < qx)) then p else q
        where q = leftLowestPoint ps
              Pt qx qy = q
              Pt px py = p

{- 2. Step 2 of Graham Scan: sort the points with respect to their angle.
   The angle of a point P is taken from the vector parallel to the x-axis 
   to the vector going from the leftLowestPoint to P. 
-}

-- Instead of angle, its cosine suffices. To avoid doubles, I take the square and the signum of x.
-- Note: a vector and the vector mirrored at the x-axis get the same value, but that doesn't matter, here.
pseudoAngleWithX :: Vr -> Ratio Int
pseudoAngleWithX (Vr 0 0) = 1  -- see note
pseudoAngleWithX (Vr x y) = (signum x * x^2) % (x^2 + y^2)
    -- Note: for our purpose the 0 vector must have a 0° (not 90°) angle with all others.
    -- Reason: the llP must be the first point in the sorted list, but the vector from
    -- base to llP is the zero vector, when llP itself is taken as base. See sort below.

-- compare 2 vectors according to their angle with x-axis
compareVectors :: Vr -> Vr -> Ordering
compareVectors a b 
    | pseudoAngleWithX a > pseudoAngleWithX b   = LT -- cosine is decreasing from 0° to 180°
    | pseudoAngleWithX a < pseudoAngleWithX b   = GT
    | normSqr a < normSqr b                     = LT
    | normSqr a > normSqr b                     = GT
    | otherwise                                 = EQ
    -- Note: when 2 vectors have the same angle with x-Axis their norm is compared.
    -- The final algorithm would not work correctly, if this were omitted.

-- sort points relative to base and angle with x-axis
sortPoints :: Pt -> [Pt] -> [Pt]
sortPoints base ps = sortBy comparePoints ps
    where comparePoints p1 p2 = compareVectors (vrFromTo base p1) (vrFromTo base p2)

{- 3. Step 3 of Graham Scan: Assume that the list of points is sorted according to step 2.
   Build the convex hull by erasing previous points of the hull, 
   which don't yield a left-turn to the new point.
-}

-- direction of 3 points
data Direction = DirLeft | DirStraight | DirRight deriving (Show, Eq)

directionOf :: Pt -> Pt -> Pt -> Direction
directionOf a b c 
    | test < 0   = DirRight
    | test > 0   = DirLeft
    | otherwise    = DirStraight
    where test = crossp2d v1 v2
          v2 = vrFromTo b c 
          v1 = vrFromTo a b

-- Assuming hull is a list containing only DirLeft directions
-- extend this hull with a new point, so that the new hull still
-- contains only DirLeft directions.
extendHull :: [Pt] -> Pt -> [Pt]
extendHull [] p   = [p]
extendHull [x] p  = if x /= p then [x,p] else [x]
extendHull hull p =
    if directionOf l1 l p == DirLeft then hull ++ [p]
                                     else extendHull (init hull) p
    where l = last hull
          l1 = last (init hull)

-- Given a list of points sorted according to step 2,
-- compute its convex hull. 
convexHullOfSorted :: [Pt] -> [Pt]
convexHullOfSorted ps
    | length ps < 2   = ps
    | otherwise       = extendHull prevHull p
    where prevHull = convexHullOfSorted (init ps)
          p = last ps

-- Given a list of points, compute its convex hull.
convexHull :: [Pt] -> [Pt]
convexHull ps
    | length ps < 2   = ps
    | otherwise       = convexHullOfSorted (sortPoints base ps)
    where base = leftLowestPoint ps

{- 4. Helper definitions for hackerrank submission -}

-- tuple to point
getPtFromIntPair :: (Int,Int) -> Pt
getPtFromIntPair (x,y) = Pt x y

-- compute perimeter

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

{- 5. Helper definitions for file IO -}

{-  Input and output files must be text and have the following format:
    First line: the number of points
    Following lines: The points. 
        Each line contains 2 strings, corresponding to x- and y-coordinate.
    Exammple: "3\n12 1\n 2 2\n -1 5"
    Note: No check, wheter number of points in first line is correct.
-}

stringListToIntlist :: [String] -> [Int]
stringListToIntlist = map (read :: String -> Int)

stringToIntListList :: String -> [[Int]]
stringToIntListList =  map stringListToIntlist . map words . lines
-- the following is correct, too:
-- stringToIntListList s =   map stringListToIntlist (map words (lines s))
-- stringToIntListList s =  (map stringListToIntlist . map words . lines) s
-- stringToIntListList s =  map stringListToIntlist . map words . lines $ s

intListListToPtList :: [[Int]] -> [Pt]
intListListToPtList = map (\ [x,y] -> Pt x y) . tail --length ignored

stringToPtList :: String -> [Pt]
stringToPtList = intListListToPtList . stringToIntListList

ptToString :: Pt -> String
ptToString (Pt x y) = show x ++ " " ++ show y

ptListToString :: [Pt] -> String
ptListToString pts = 
    unlines ( len : map ptToString pts ) where
        len = show $ length pts

-- |Apply convexHull function on input given as string
--  and return result as string 
processInputString :: String -> String
processInputString = ptListToString . convexHull . stringToPtList
                     