module DeltaSigma.Misc where

import Graphics.Gnuplot.Simple

type Modulator = (Double -> Double) -> Double -> [Double] -> [Double]

quantize :: Double -> Double
quantize y = if y >= 0 then 1 else -1

nextpow2 :: Int -> Int
nextpow2 x = (2 ^) . ceiling $ log2 x
  where
    log2 x = log (fromIntegral x) / log 2

window :: (Int -> [Double]) -> [Double] -> [Double]
window win xs = zipWith (*) (win (length xs)) xs

plotPsd :: [Double] -> IO ()
plotPsd xs = plotList [ Custom "logscale x" []
                      , Custom "grid" []
                      ] xs
