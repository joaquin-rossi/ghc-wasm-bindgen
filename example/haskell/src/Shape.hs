module Shape where

import Data.List (maximumBy)
import Data.Ord (comparing)

data Shape
    = Circle Double
    | Rect { width :: Double, height :: Double }

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rect w h) = w * h

scale :: Double -> Shape -> Shape
scale k (Circle r) = Circle (k * r)
scale k (Rect w h) = Rect (k * w) (k * h)

isSquare :: Shape -> Bool
isSquare (Rect w h) = w == h
isSquare (Circle _) = False

totalArea :: [Shape] -> Double
totalArea = sum . map area

largest :: [Shape] -> Maybe Shape
largest [] = Nothing
largest shapes = Just (maximumBy (comparing area) shapes)
