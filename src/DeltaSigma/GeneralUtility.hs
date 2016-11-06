module DeltaSigma.GeneralUtility (dbv, dbp, undbv, undbp, dbm, undbm) where

log10 :: (Floating a) => a -> a
log10 x = log x / log 10

dbv :: (Floating a) => a -> a
dbv x = 20 * log10 x

dbp :: (Floating a) => a -> a
dbp x = 10 * log10 x

undbv :: (Floating a) => a -> a
undbv x = 10 ** (x / 20)

undbp :: (Floating a) => a -> a
undbp x = 10 ** (x / 10)

dbm :: (Floating a) => a -> a -> a
dbm r v = 10 * log10 (v ** 2 / r) + 30

undbm :: (Floating a) => a -> a -> a
undbm r x = sqrt $ 10 ** ((x - 30) / 10) 
