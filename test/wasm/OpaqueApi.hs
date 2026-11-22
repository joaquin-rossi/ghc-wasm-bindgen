module OpaqueApi
  ( Secret, Other, Public (..), Choice (..)
  , makeSecret, readSecret, sameSecret, bumpSecret, readWith
  , makeOther, readOther, failMake, failRead, lateFail
  , publicCount, echoPublic, choose
  ) where

-- A function field deliberately cannot be serialized or derive Generic JSON.
data Secret = Secret (Int -> Int)
newtype Other = Other Int
data Public = Public { count :: Int, label :: String }
data Choice = First | Second

makeSecret :: Int -> Secret
makeSecret n = Secret (const n)

readSecret :: Secret -> Int
readSecret (Secret f) = f 0

sameSecret :: Secret -> Secret
sameSecret = id

bumpSecret :: Secret -> Int -> Secret
bumpSecret s n = makeSecret (readSecret s + n)

readWith :: Secret -> [Int] -> Int
readWith s ns = readSecret s + sum ns

makeOther :: Int -> Other
makeOther = Other

readOther :: Other -> Int
readOther (Other n) = n

failMake :: Int -> Secret
failMake _ = error "cannot create secret"

failRead :: Secret -> Int
failRead _ = error "cannot read secret"

lateFail :: Int -> Secret
lateFail _ = Secret (\_ -> error "lazy secret failed")

publicCount :: Public -> Int
publicCount = count

echoPublic :: Public -> Public
echoPublic = id

choose :: Bool -> Choice
choose True = First
choose False = Second
