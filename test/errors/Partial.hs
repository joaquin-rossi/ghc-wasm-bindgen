module Partial (Partial (One), make) where
data Partial = One Int | Two Int
make :: Int -> Partial
make = Two
