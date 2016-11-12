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
calculateSNR f nsig hwfft = dbp (s / n)
  where
    norm xs = sum $ fmap (** 2) xs 
    s = norm $ fmap (hwfft !!) [f-nsig .. f+nsig]
    n = norm $ fmap (hwfft !!) ([0 .. f-nsig-1] ++ [f+nsig+1 .. length hwfft - 1])
    

partitionABCD = undefined
infnorm = undefined
impL1 = undefined
pulse = undefined
rmsGain = undefined
