module DeltaSigma.DeltaSigmaUtility (calculateSNR
                                   , partitionABCD
                                   , infnorm
                                   , impL1
                                   , pulse
                                   , rmsGain
                                   ) where

import DeltaSigma.Type
import DeltaSigma.GeneralUtility

calculateSNR :: Bin -> Bin -> [Double] -> Double
calculateSNR f nsig hwfft = dbv (s / n)
  where
    norm xs = sum $ fmap (** 2) xs 
    s = norm . take (2 * nsig + 1) $ drop (f - nsig)  hwfft
    n = norm hwfft - s 

partitionABCD = undefined
infnorm = undefined
impL1 = undefined
pulse = undefined
rmsGain = undefined
