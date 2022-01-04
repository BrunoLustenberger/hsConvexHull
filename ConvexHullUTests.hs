{- |
Module : ConvexHullUTests
Description : Unit tests for module ConvexHull
Copyright : (c) 2021 Bruno M.S. Lustenberger

You can execute these tests from ghci by loading this module and then enter:
-- for a single testcase, e.g. pa0
*ConvexHullUTests> runTestTT pa0
-- for a group of testcases, e.g. pseudoAngleTests 
*ConvexHullUTests> runTestTT pseudoAngleTests
-- for all testcases 
*ConvexHullUTests> runTestTT allTestCases

-}
module ConvexHullUTests where

import Test.HUnit
import Data.Ratio ( (%) )

import ConvexHull
import Utils

{- Points used in the tests -}

p00 = Pt 0 0
p01 = Pt 0 1
p02 = Pt 0 2
p03 = Pt 0 3
p10 = Pt 1 0
p11 = Pt 1 1
p12 = Pt 1 2
p13 = Pt 1 3
p20 = Pt 2 0
p21 = Pt 2 1
p22 = Pt 2 2
p23 = Pt 2 3
p30 = Pt 3 0
p31 = Pt 3 1
p32 = Pt 3 2
p33 = Pt 3 3

p25 = Pt 2 5
p26 = Pt 2 6
p45 = Pt 4 5
p51 = Pt 5 1
p53 = Pt 5 3
p5_10 = Pt 5 10
p62 = Pt 6 2
p77 = Pt 7 7

p1m3  = Pt 1 (-3)
p1m1  = Pt 1 (-1)
p34 = Pt 3 4
p3m2  = Pt 3 (-2)
p4m2  = Pt 4 (-2)
p52 = Pt 5 2

pm12  = Pt (-1) 2
pm24  = Pt (-2) 4
pm1m1  = Pt (-1) (-1)
pm2m1  = Pt (-2) (-1)
pm32  = Pt (-3) 2
pm3m2  = Pt (-3) (-2)
pm33  = Pt (-3) 3
pm21  = Pt (-2) 1


{- Trigonometry -}

v20 = vrFromTo (Pt 2 1) (Pt 4 1)
v11 = Vr 1 1
cp1 = crossp2d v20 v11
tr1 = TestCase $ assertEqual "tr1" cp1 2
cp1' = crossp2d v11 v20
tr1' = TestCase $ assertEqual "tr1'" cp1' (-2)
tr2 = TestCase $ assertEqual "tr2"   (crossp2d (Vr 0 (-1)) (Vr (-1) 0)) (-1)
tr3 = TestCase $ assertEqual "tr3"   (normSqr (Vr 3 4)) 25

trigoTests = TestLabel "trigo" (TestList [tr1,tr1',tr2,tr3])

{- leftLowestPoint -}

set0 = []
set1 = [p11]
set2 = [p1m1,p3m2]
set2' = [pm2m1,p31]
set2'' = [p1m1,pm2m1]
set3 = [p31,p1m1,pm2m1]
set4 = [p11,pm24,p4m2]
set4' = [p11,p4m2,p4m2,pm24,p4m2]

set15 = [p00,p11,p31,p13,p34,p52,pm24,pm12,pm2m1,
         pm32,pm3m2,p1m3,p3m2,p1m1,p4m2]
--sorted  p1m3,p4m2,p3m2,p52,p31,p34,p1m1,p11,p13,
--        p00,pm12,pm24,pm32,pm2m1,pm3m2

ll1   = TestCase $ assertEqual "ll1" (leftLowestPoint set1) p11
ll2   = TestCase $ assertEqual "ll2" (leftLowestPoint set2) p3m2
ll2'  = TestCase $ assertEqual "ll2'" (leftLowestPoint set2') pm2m1
ll2'' = TestCase $ assertEqual "ll2''" (leftLowestPoint set2'') pm2m1
ll3   = TestCase $ assertEqual "ll3" (leftLowestPoint set3) pm2m1
ll4   = TestCase $ assertEqual "ll4" (leftLowestPoint set4) p4m2
ll4'  = TestCase $ assertEqual "ll4'" (leftLowestPoint set4') p4m2
ll15  = TestCase $ assertEqual "ll15" (leftLowestPoint set15) p1m3

leftLowestTests = TestLabel "leftLowest" 
    (TestList [ll1,ll2,ll2',ll3,ll4,ll4',ll15])

{- pseudoAngle -}

a1 = 1 % 5
a3 = 1 % 2

pa0    = TestCase $ assertEqual "pa0"     (pseudoAngleWithX (Vr 0 0))     1 
    -- note the special definition for this special case!!!
pa0'   = TestCase $ assertEqual "pa0'"    (pseudoAngleWithX (Vr 2 0))     1
pa0''  = TestCase $ assertEqual "pa0''"   (pseudoAngleWithX (Vr (-1) 0))  (-1)
pa1'   = TestCase $ assertEqual "pa1'"    (pseudoAngleWithX (Vr 1 2))     a1
pa1''  = TestCase $ assertEqual "pa1''"   (pseudoAngleWithX (Vr (-1) 2))  (-a1)
pa2    = TestCase $ assertEqual "pa2"     (pseudoAngleWithX (Vr 2 4))     a1
pa2'   = TestCase $ assertEqual "pa2'"    (pseudoAngleWithX (Vr 3 6))     a1
pa3'   = TestCase $ assertEqual "pa3'"    (pseudoAngleWithX (Vr 1 1))     a3
pa3''  = TestCase $ assertEqual "pa3''"   (pseudoAngleWithX (Vr 2 2))     a3
pa3''' = TestCase $ assertEqual "pa3'''"  (pseudoAngleWithX (Vr 3 3))     a3
  -- when using cosine and double instead of square and Ratio int, 
  -- this test fails

pseudoAngleTests = TestLabel "pseudoAngle" 
    (TestList [pa0,pa0',pa0'',pa1',pa1'',pa2,pa2',pa3',pa3'',pa3'''])

{- comparing vectors -}

cv0  = TestCase $ assertEqual "cv0" (compareVectors (Vr 1 1) (Vr 1 (-1)))    EQ
cv1  = TestCase $ assertEqual "cv1" (compareVectors (Vr 3 1) (Vr 2 1))       LT
cv2  = TestCase $ assertEqual "cv2" (compareVectors (Vr (-4) 1) (Vr (-2) 1)) GT
cv3  = TestCase $ assertEqual "cv3" (compareVectors (Vr (-4) 1) (Vr 2 1))    GT
cv4  = TestCase $ assertEqual "cv4" (compareVectors (Vr 2 1) (Vr 4 2))       LT
cv5  = TestCase $ assertEqual "cv5" (compareVectors (Vr 3 3) (Vr 2 2))       GT

comparingVectorsTests = TestLabel "comparingVectors" 
    (TestList [cv0,cv1,cv2,cv3,cv4,cv5])

{- sorting points -}

sp0 =   TestCase $ assertEqual "sp0"   (sortPoints p00 []) []
sp1 =   TestCase $ assertEqual "sp1"   (sortPoints p11 [p11]) [p11]
sp2 =   TestCase $ assertEqual "sp2"   (sortPoints p11 [p34,p52]) [p52,p34]
sp3 =   TestCase $ assertEqual "sp3"   
            (sortPoints p00 [p13,p33,p23,p22,pm12,pm24,p11,p30])
            [p30,p11,p22,p33,p23,p13,pm12,pm24]
sp4 =   TestCase $ assertEqual "sp4"   
            (sortPoints p01 [p13,p01,p32,p01,p32]) 
            [p01,p01,p32,p32,p13]
sp15 =  TestCase $ assertEqual "sp15" 
            (sortPoints p1m3 set15) 
            [p1m3,p4m2,p3m2,p52,p31,p34,p1m1,p11,p13,p00,
             pm12,pm24,pm32,pm2m1,pm3m2]

sortingPointsTests = TestLabel "sortingPoints" 
    (TestList [sp0,sp1,sp2,sp3,sp4,sp15])

{- extendHull -}

eh0   = TestCase $ assertEqual "eh0"  
            (extendHull [] p1m3) [p1m3]
eh1   = TestCase $ assertEqual "eh1"  
            (extendHull [p11] pm2m1) [p11,pm2m1]
eh2   = TestCase $ assertEqual "eh2"  
            (extendHull [p11,p52] p34) [p11,p52,p34]
eh2'  = TestCase $ assertEqual "eh2'" 
            (extendHull [p11,p52] p4m2) [p11,p4m2]
eh3   = TestCase $ assertEqual "eh3"  
            (extendHull [p00,p1m1,p4m2] p31) [p00,p1m1,p4m2,p31]
eh3'  = TestCase $ assertEqual "eh3'" 
            (extendHull [p00,p1m1,p31] p52) [p00,p1m1,p52]
eh4   = TestCase $ assertEqual "eh4"  
            (extendHull [p00,p1m1,p31,p13] p52) [p00,p1m1,p52]

extendHullTests = TestLabel "extendHull" 
    (TestList [eh0,eh1,eh2,eh2',eh3,eh3',eh4])

{- convexHull -}

chs = TestCase $ assertEqual "chs" 
        (convexHullOfSorted [p00,p31,p52,p11,p34,p13,pm12,pm24,pm32]) 
        [p00,p31,p52,p34,pm24,pm32]

ch0     = TestCase $ assertEqual "ch0"    (convexHull []) []
ch1     = TestCase $ assertEqual "ch1"    (convexHull [p10]) [p10]
ch1'    = TestCase $ assertEqual "ch1'"   (convexHull [p10,p10]) [p10]
ch1''   = TestCase $ assertEqual "ch1''"  (convexHull [p10,p10,p10]) [p10]
ch1'''  = TestCase $ assertEqual "ch1'''" (convexHull [p10,p10,p11]) [p10,p11]
ch2     = TestCase $ assertEqual "ch2"    (convexHull [p20,p10]) [p10,p20]
ch3     = TestCase $ assertEqual "ch3"    
            (convexHull [p01,p00,p10]) [p00,p10,p01]
ch4     = TestCase $ assertEqual "ch4"    
            (convexHull [p01,p00,p02,p11]) [p00,p11,p02]
ch5     = TestCase $ assertEqual "ch5"    
            (convexHull [p00,p10,p20,p01,p02]) [p00,p20,p02]
ch6     = TestCase $ assertEqual "ch6"    
            (convexHull [p00,p10,p20,p01,p12,p11]) [p00,p20,p12,p01]
ch7     = TestCase $ assertEqual "ch7"    
            (convexHull [p20,p21,p22,p00,p01,p02,p10,p11,p12]) 
            [p00,p20,p22,p02]
ch7'    = TestCase $ assertEqual "ch7'"   
            (convexHull [p20,p21,p22,p00,p01,p02,p10,p11,p12,  
                         p20,p00,p11,p21,p02]) 
            [p00,p20,p22,p02]
ch7''   = TestCase $ assertEqual "ch7''"   
            (convexHull [p00,p10,p20,p12,pm1m1,pm21,pm33]) 
            [pm1m1,p20,p12,pm33]
ch8     = TestCase $ assertEqual "ch8"    
            (convexHull [p20,p21,p22,p00,p01,p02,p03,p10,p11,p12])
            [p00,p20,p22,p03]
ch8'    = TestCase $ assertEqual "ch8'"   
            (convexHull [p20,p21,p22,p00,p01,p02,p03,p10,p11,p12,  
                         p11,p03,p02,p12,p22]) 
            [p00,p20,p22,p03]
ch15    = TestCase $ assertEqual "ch15"  
            (convexHull set15) 
            [p1m3,p4m2,p52,p34,pm24,pm32,pm3m2]

chx0    = TestCase $ assertEqual "chx0"    
            (convexHull [p00,p11,p22]) [p00,p22]
chx1    = TestCase $ assertEqual "chx1"    
            (convexHull [p00,p11,p22,p33,p30]) [p00,p30,p33]
chx2    = TestCase $ assertEqual "chx2"    
            (convexHull [p00,p11,p22,p33,p30,p02]) [p00,p30,p33,p02]

cw2014  = TestCase $ assertEqual "cw2014"  
            (convexHull [p62,p22,p33,p00,p77,p5_10,pm32,p51,p11,p26])
            [p00,p51,p62,p77,p5_10,pm32]

reimai  = TestCase $ assertEqual "reimai"  
            (convexHull [p20, p11, p12, p23, p00, p01, p30, p31,
                         p02, p03, p32, p33, p10, p21, p22, p13])
            [p00,p30,p33,p03]

convexHullTests = TestLabel "convexHull" 
    (TestList [
        chs,ch0,ch1,ch1',ch1'',ch1''',ch2,ch3,ch4,ch5,ch6,ch7,
        ch7',ch8,ch8',ch15,
        chx0,chx1,chx2,
        cw2014,reimai])

{- hackerrank -}

hr00  = TestCase $ assertEqual "hr00" 
            (convexHull [p11,p25,p33,p53,p32,p22]) [p11,p53,p25]
hr01  = TestCase $ assertEqual "hr01" 
            (convexHull [p32,p25,p45]) [p32,p45,p25]

hr00Len = lengthOfClosedPath (convexHull [p11,p25,p33,p53,p32,p22])
hr01Len = lengthOfClosedPath (convexHull [p32,p25,p45])
hr00L = TestCase $ assertEqual "hr00L" (round (hr00Len * 10)) 122
    -- perimeter rounded to 1 dec is 12.2    
hr01L = TestCase $ assertEqual "hr01L" (round (hr01Len * 10)) 83
    -- perimeter rounded to 1 dec is 8.3    

hackerRankTests = TestLabel "hackerRank" (TestList [hr00,hr01,hr00L,hr01L])

{- all tests -}

allTestCases = TestList [
    trigoTests, leftLowestTests, pseudoAngleTests, comparingVectorsTests, 
    sortingPointsTests, extendHullTests, convexHullTests, hackerRankTests]
                         
