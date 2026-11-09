-- Unsupported APIs must be rejected with a clear message.
module Errors (tests) where

import Control.Monad (forM)
import Data.List (isInfixOf)

import GhcWasmBindgen.Inspect (InspectOptions (..), Source (..), inspect)

cases :: [(FilePath, String)]
cases =
  [ ("HigherOrder.hs", "function-typed values cannot cross the boundary")
  , ("Operator.hs", "operators cannot be exported")
  , ("Polymorphic.hs", "polymorphic functions are not supported")
  , ("Unexported.hs", "Hidden: used by the API, so the type and all its constructors must be exported")
  , ("UsesIO.hs", "IO is not supported yet")
  ]

tests :: IO Bool
tests = fmap and $ forM cases $ \(file, needle) -> do
  res <- inspect (InspectOptions ("test/errors/" ++ file) (SourceFiles []))
  case res of
    Left errs | any (needle `isInfixOf`) errs -> True <$ putStrLn ("ok   " ++ file)
    Left errs -> False <$ putStrLn ("FAIL " ++ file ++ ": expected " ++ show needle ++ ", got:\n" ++ unlines errs)
    Right _ -> False <$ putStrLn ("FAIL " ++ file ++ " was accepted")
