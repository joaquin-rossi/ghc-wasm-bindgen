module NestedField (Secret, Box (..)) where
data Secret = Secret (Int -> Int)
data Box = Box { secret :: Secret }
