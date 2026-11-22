module Nested (Secret, many, optional, alternative, pair, consume, values, keys) where
import qualified Data.Map.Strict as Map
data Secret = Secret Int
many :: Secret -> [Secret]
many s = [s]
optional :: Secret -> Maybe Secret
optional = Just
alternative :: Secret -> Either String Secret
alternative = Right
pair :: Secret -> (Int, Secret)
pair s = (0, s)
consume :: [Secret] -> Int
consume = length
values :: Secret -> Map.Map Int Secret
values = Map.singleton 0
keys :: Map.Map Secret Int -> Int
keys = Map.size
