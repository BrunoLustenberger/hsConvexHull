{- |
Module : UtilsUTests
Description : Unit tests for module Utils
Copyright : (c) 2022 Bruno M.S. Lustenberger
See ConvexHullUTests for how to execute these tests.
-}
module ConvexHullUTests where

import Test.HUnit

import ConvexHull (Pt(..))
import Utils


{- hackerrank -}

-- nothing yet

{- file IO -}

io00 = TestCase $ assertEqual "io00" 
            (stringToIntListList "3\n12 1\n2 2\n-1 5\n") 
            [[3],[12,1],[2,2],[(-1),5]]
io01 = TestCase $ assertEqual "io01" 
            (intListListToPtList [[3],[12,1],[2,2],[(-1),5]]) 
            [Pt 12 1, Pt 2 2,Pt (-1) 5]
io02 = TestCase $ assertEqual "io02"
            (ptToString (Pt 2 (-3))) "2 -3"
io03 = TestCase $ assertEqual "io03"
            (ptListToString [Pt 12 1, Pt 2 2,Pt (-1) 5]) 
            "3\n12 1\n2 2\n-1 5\n"

ioTests = TestLabel "io" (TestList [io00,io01,io02,io03])

{- all tests -}

allTestCases = TestList [ioTests]
                         
