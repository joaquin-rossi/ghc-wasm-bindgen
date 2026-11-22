module NamesApi
  ( Secret', Value' (..), Choice' (..)
  , makeSecret, readSecret, sameSecret
  , echoValue, echoValues, echoChoice, echoChoices
  ) where

data Secret' = Secret Int
data Value' = Value' { field' :: Int } | Empty' | Positional' Int
data Choice' = First' | Second'

makeSecret :: Int -> Secret'
makeSecret = Secret

readSecret :: Secret' -> Int
readSecret (Secret n) = n

sameSecret :: Secret' -> Secret'
sameSecret = id

echoValue :: Value' -> Value'
echoValue = id

echoValues :: [Value'] -> [Value']
echoValues = id

echoChoice :: Choice' -> Choice'
echoChoice = id

echoChoices :: [Choice'] -> [Choice']
echoChoices = id
