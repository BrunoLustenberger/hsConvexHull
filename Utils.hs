{- |
Module : Utils
Description : Utilities for calling convexHull.
Copyright : (c) 2022 Bruno M.S. Lustenberger
-}
module Utils where

import ConvexHull (Pt(..), Vr(..), vrFromTo)

{- 1. Helper definitions for hackerrank submission -}

type IntPair = (Int,Int)

-- IntPair to point
getPtFromIntPair :: IntPair -> Pt
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

{- 2. Helper definitions for file IO -}

-- Note: No check, whether number of points in first line is correct.

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

intListListToIntPairList :: [[Int]] -> [IntPair]
intListListToIntPairList = map (\ [x,y] -> (x,y)) . tail --length ignored

stringToIntPairList :: String -> [IntPair]
stringToIntPairList = intListListToIntPairList . stringToIntListList

ptToString :: Pt -> String
ptToString (Pt x y) = show x ++ " " ++ show y

ptListToString :: [Pt] -> String
ptListToString pts = 
    unlines ( len : map ptToString pts ) where
        len = show $ length pts
