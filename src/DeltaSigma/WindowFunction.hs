module DeltaSigma.WindowFunction (rect, gauss, hann, blackman) where

rect :: Int -> [Double]
rect n = replicate n 1.0

gauss :: Double -> Int -> [Double]
gauss sig n' = [exp (- ((x-0.5)/sig)**2) | x <- (/ (n-1)) <$> [0 .. n-1]]
  where
    n = fromIntegral n'

hann :: Int -> [Double]
hann n' = [0.5 - 0.5 * cos (2*pi*x) | x <- (/ (n-1)) <$> [0 .. n-1]]
  where
    n = fromIntegral n'

blackman :: Int -> [Double]
blackman n' = [0.42 - 0.5 * cos (2*pi*x) + 0.08 * cos (4*pi*x)
                | x <- (/ (n-1)) <$> [0 .. n-1]]
  where
    n = fromIntegral n'
