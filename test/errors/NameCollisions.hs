module NameCollisions
  ( Secret', Secret_, Value' (..), Value_ (..), Fields (..)
  , encodeValue_, f', f_
  ) where

data Secret' = SecretPrime Int
data Secret_ = SecretUnderscore Int
data Value' = Build' Int
data Value_ = Build_ Int
data Fields = Fields { field' :: Int, field_ :: Int }

encodeValue_ :: Int -> Int
encodeValue_ = id

f', f_ :: Int -> Int
f' = id
f_ = id
