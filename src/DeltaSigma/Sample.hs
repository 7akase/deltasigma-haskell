-- module DeltaSigma.Sample where

import DeltaSigma.Type
import DeltaSigma.DeltaSigmaUtility
import DeltaSigma.GeneralUtility
import DeltaSigma.PowerSpectrumDensity
import DeltaSigma.WindowFunction
import DeltaSigma.Misc
import TransferFunction

import Graphics.Gnuplot.Simple

getT :: [[Double]] -> [Double]
getT = fmap (!! 0)
getU :: [[Double]] -> [Double]
getU = fmap (!! 1)
getV :: [[Double]] -> [Double]
getV = fmap (!! 2)

waveForSnr :: Int -> Double -> Modulator -> [Double] -> [Double] 
waveForSnr osr ts m i = take n_fft . getV $ iterate (m df ts) i
  where
    df t    = 0.7 * sin (w_sig * t)
    bin_sig = 23 
    n_fft   = nextpow2 (bin_sig * osr)
    t_fft   = fromIntegral n_fft * ts
    t_sig   = t_fft / fromIntegral bin_sig
    w_sig   = 2 * pi / t_sig

dsm1 :: Modulator
dsm1 df dt [t_prev, zu, zv, zy] = [t, u, v, y]
  where
    t  = t_prev + dt
    u  = df t
    y  = zy + (u - v)
    v  = quantize zy

pdc :: Modulator
pdc f dt [t_prev, zu, zv, zy, zfb] = [t, u, v, y, fb]
  where
    t  = t_prev + dt
    u  = f t
    [_, y] = solve (laglead (0.01, 0.5)) t_prev [zu-zfb, zy] t
    v  = quantize zy
    fb = zfb + 2*v - zv

main :: IO ()
main = do
  let osr = 256
      ts = 1
      vs = waveForSnr osr ts dsm1 [0, -1, 0, 0]
      vs_psd = psd $ window rect vs
  -- putStrLn . show . calculateSNR 23 3 $ take osr vs_psd
  -- plotList [] $ vs 
  -- plotPsd $ fmap dbv vs_psd
  -- plotList [] $ waveForSnr osr ts pdc
  plotPsd . fmap dbv . psd . window rect $ waveForSnr osr ts pdc [0, -1, 0, 0, 0]
  putStrLn "fin."
