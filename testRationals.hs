-- some tests with the type Rational
import Data.Ratio

r1 = 2 % 3
r2 = 4 % 5
r3 = r1+r2
r4 = r1-r2
r5 = r1*r2
r6 = r1/r2

pseudoAngleWithX :: (Int, Int) -> Rational
pseudoAngleWithX (0,0) = 1
pseudoAngleWithX (x,y) = xr^2 / (xr^2 + yr^2) * signum xr
    where xr = toRational x
          yr = toRational y

a0 = pseudoAngleWithX (0,0)
a1 = pseudoAngleWithX (3,0)
a2 = pseudoAngleWithX (3,1)
a2' = pseudoAngleWithX (12,4)
a2'' = pseudoAngleWithX ((-6),2)
a3 = pseudoAngleWithX (1,1)
a3' = pseudoAngleWithX (2,2)
a3'' = pseudoAngleWithX (3,3)

b1 = approxRational (sqrt 2) 0.01
b2 = approxRational (sqrt 2) 0.000001

r2d :: Rational -> Double
r2d r = fromIntegral (numerator r) / fromIntegral (denominator r)

c1 = r2d (1 % 3)
c2 = r2d (2 % 14)

b1' = r2d b1
b2' = r2d b2

pi5 = approxRational pi 0.000005
pi5' = r2d pi5
