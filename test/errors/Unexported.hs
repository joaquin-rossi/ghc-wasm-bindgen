module Unexported (make) where
data Hidden = Hidden { n :: Int }
make :: Int -> Hidden
make = Hidden
