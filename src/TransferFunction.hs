module TransferFunction where

import Numeric.GSL.ODE
import Numeric.LinearAlgebra
-- import Numeric.LinearAlgebra (linspace, toLists)

type Time = Double
type Freq = Double

solve :: (Time -> [Double] -> [Double]) -> Time -> [Double] -> Time -> [Double]
solve xdot t0 x0 t1 = last . toLists $ odeSolve xdot x0 ts
  where
    ts = linspace 2 (t0, t1)

diffStepFunc t = if 0.0 <= t && t <= trf then 1 / trf else 0
  where
    trf = 0.01
diffSinFunc fsig t = 2*pi*cos(2*pi*fsig*t)

lpf1 :: Freq -> (Time -> Double) -> Time -> [Double] -> [Double]
lpf1 fp xdot t [x, y] = [sx, sy]
  where
    sx = xdot t 
    sy = (x - y) / t1 -- Y/X = 1 / (1 + s.t1) <=> Y + t1.sY = X
    t1 = 1 / 2 / pi / fp

lpf2 :: (Freq, Freq) -> (Time -> Double) -> Time -> [Double] -> [Double]
lpf2 (fp1, fp2) xdot t [x, y, sy] = [sx, sy, ssy]
  where
    sx  = xdot t 
    ssy = x - y - (t1 + t2) * sy
    t1  = 1 / 2 / pi / fp1
    t2  = 1 / 2 / pi / fp2
    --     H(s) = 1 / (1 + s/wp1) * (1 + s/wp2)
    -- <=> Y + (t1+t2).sY + t1.t2.ssY = X
    -- <=> t1.t2.ssY = X - Y - (t1+t2).sY

hpf1 :: Freq -> (Time -> Double) -> Time -> [Double] -> [Double]
-- true 1-zero tranfer function can't be solved
hpf1 fz xdot t [x, y] = [sx, sy]
  where
    sx  = xdot t 
    sy  = (tz*sx - y) / tz
    tz  = 1 / 2 / pi / fz
 
laglead :: (Freq, Freq) -> (Time -> Double) -> Time -> [Double] -> [Double]
laglead (fp, fz) xdot t [x, y] = [sx, sy]
  where
    sx  = xdot t 
    sy  = (x + tz*sx - y) / tp
    tp  = 1 / 2 / pi / fp
    tz  = 1 / 2 / pi / fz
    -- H(x) = (1 + tz.s) / (1 + tp.s)
    -- <=> Y + tp.sY = X + tz.sX
    -- <=> sY = (X + tz.sX - Y) / tp

p2p :: [Double] -> Double
p2p xs = maximum xs - minimum xs
