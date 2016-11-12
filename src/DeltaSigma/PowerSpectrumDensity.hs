module DeltaSigma.PowerSpectrumDensity where

import Numeric.FFT
import Data.Complex

psd :: [Double] -> [Double]
psd xs = take n_half . fmap (/ (fromIntegral n_half))
        . fmap magnitude . fft . fmap (:+ 0) $ take n xs
  where
    n = length xs
    n_half = n `div` 2
