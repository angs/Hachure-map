-- Bicubic interpolation algorithm from http://books.google.fi/books?id=1aAOdzK3FegC&pg=PA136&dq=bicubic+interpolation&hl=fi&sa=X&ei=japrUaWaH8Tcsga5hYHoCg&redir_esc=y#v=onepage&q=bicubic%20interpolation&f=false

module Gradient (gradientStepper) where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import Types

bcuMatrix = (16><16) 
  [
   1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  -3, 0, 0, 3, 0, 0, 0, 0,-2, 0, 0,-1, 0, 0, 0, 0,
   2, 0, 0,-2, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
   0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
   0, 0, 0, 0,-3, 0, 0, 3, 0, 0, 0, 0,-2, 0, 0,-1,
   0, 0, 0, 0, 2, 0, 0,-2, 0, 0, 0, 0, 1, 0, 0, 1,
  -3, 3, 0, 0,-2,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0,-3, 3, 0, 0,-2,-1, 0, 0,
   9,-9, 9,-9, 6, 3,-3,-6, 6,-6,-3, 3, 4, 2, 1, 2,
  -6, 6,-6, 6,-4,-2, 2, 4,-3, 3, 3,-3,-2,-1,-1,-2,
   2,-2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 2,-2, 0, 0, 1, 1, 0, 0,
  -6, 6,-6, 6,-3,-3, 3, 3,-4, 4, 2,-2,-2,-2,-1,-1,
   4,-4, 4,-4, 2, 2,-2,-2, 2,-2,-2, 2, 1, 1, 1, 1::Double
  ]

--  0  1  2  3
--  4  5  6  7
--  8  9 10 11
-- 12 13 14 15
--
-- 01  (0,0) --> (0,1)   f, dx, dy, cross
-- 32    |    y=x1, x=x2 
--       v
--     (1,0)
centeredDifferencing = (16><16) [ 
--    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15
      0,   0,   0,   0,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   1,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,   1,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,-1/2,   0, 1/2,   0,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,-1/2,   0, 1/2,   0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,   0,-1/2,   0, 1/2,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,-1/2,   0, 1/2,   0,   0,   0,   0,   0,
      0,-1/2,   0,   0,   0,   0,   0,   0,   0, 1/2,   0,   0,   0,   0,   0,   0,
      0,   0,-1/2,   0,   0,   0,   0,   0,   0,   0, 1/2,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,-1/2,   0,   0,   0,   0,   0,   0,   0, 1/2,   0,
      0,   0,   0,   0,   0,-1/2,   0,   0,   0,   0,   0,   0,   0, 1/2,   0,   0,
      1/4, 0,-1/4,   0,   0,   0,   0,   0,-1/4,   0, 1/4,   0,   0,   0,   0,   0,
      0, 1/4,   0,-1/4,   0,   0,   0,   0,   0,-1/4,   0, 1/4,   0,   0,   0,   0,
      0,   0,   0,   0,   0, 1/4,   0,-1/4,   0,   0,   0,   0,   0,-1/4,   0, 1/4,
      0,   0,   0,   0, 1/4,   0,-1/4,   0,   0,   0,   0,   0,-1/4,   0, 1/4,   0::Double
  ]

combinedMatrix = bcuMatrix <> centeredDifferencing

-- bicubic interpolation 
-- returns (value, d/dx, d/dy) at (y,x)@m
bicubicInterpolation :: Matrix Double -> (Double, Double) -> (Double, Double, Double)
bicubicInterpolation m (y, x) = (val, g1, g2)
  where
  (x_, dx) = properFraction x --u
  (y_, dy) = properFraction y --t
  ls  = tr $ reshape 4 $ combinedMatrix #> flatten sub
  us  = 4 |> [1.0,  dx,  dx*dx,  dx*dx*dx]
  ts  = 4 |> [1.0,  dy,  dy*dy,  dy*dy*dy]
  us2 = 4 |> [0.0, 1.0, 2.0*dx, 3.0*dx*dx]
  ts2 = 4 |> [0.0, 1.0, 2.0*dy, 3.0*dy*dy]
  val = ts  `dot` (ls #> us)  --f
  g1  = ts2 `dot` (ls #> us)  --d/dy
  g2  = ts  `dot` (ls #> us2) --d/dx
  sub = subMatrix (y_-1,x_-1) (4,4) m

gradientStepper:: Direction -> Double -> Matrix Double -> (Double, Double) -> [(Double, Double, Double)]
gradientStepper dir delta mat (y0, x0) = (x0, y0, z0) : ga x0 y0 z0 dy0 dx0
  where
  (z0, dy0, dx0) = bicubicInterpolation mat (y0, x0)
  ga x y z dy dx
    | x' < 2 || y' < 2 || x' > fromIntegral (cols mat - 3) || y' > fromIntegral (rows mat - 3) = []
    | z' > z = (x', y', z') : ga x' y' z' dy' dx'
    | otherwise = []
    where
    l 
      |  dy == 00 && dx == 0.0 = 1
      | otherwise = 1.0 / sqrt (dy*dy+dx*dx)
    (dyn, dxn) = (dy*l, dx*l)
    directionOp = if dir == Descent then subtract else (+)
    (y',x') = (y `directionOp` delta*dyn, x `directionOp` delta*dxn)
    (z',dy',dx') = bicubicInterpolation mat (y', x') 

