module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

import qualified Errors

-- | Each group prints its own results and returns whether all passed.
groups :: [(String, IO Bool)]
groups =
  [ ("errors", Errors.tests)
  ]

main :: IO ()
main = do
  results <- mapM (\(name, run) -> putStrLn ("# " ++ name) >> run) groups
  unless (and results) exitFailure
