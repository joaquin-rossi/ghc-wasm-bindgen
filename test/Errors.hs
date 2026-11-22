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
  , ("Partial.hs", "Partial(Two)")
  , ("Nested.hs", "many: opaque type Secret is only supported as a direct function argument or result")
  , ("Nested.hs", "optional: opaque type Secret")
  , ("Nested.hs", "alternative: opaque type Secret")
  , ("Nested.hs", "pair: opaque type Secret")
  , ("Nested.hs", "consume: opaque type Secret")
  , ("Nested.hs", "values: opaque type Secret")
  , ("Nested.hs", "keys: opaque type Secret")
  , ("NestedField.hs", "Box.Box field secret: opaque type Secret")
  , ("AbstractParameterized.hs", "types with parameters are not supported yet")
  , ("NameCollisions.hs", "type Secret'")
  , ("NameCollisions.hs", "type Secret_")
  , ("NameCollisions.hs", "type Value'")
  , ("NameCollisions.hs", "constructor Build'")
  , ("NameCollisions.hs", "encode for Value'")
  , ("NameCollisions.hs", "function f'")
  , ("NameCollisions.hs", "Fields.Fields constructor parameters")
  ]

tests :: IO Bool
tests = fmap and $ forM cases $ \(file, needle) -> do
  res <- inspect (InspectOptions ("test/errors/" ++ file) (SourceFiles []))
  case res of
    Left errs | any (needle `isInfixOf`) errs -> True <$ putStrLn ("ok   " ++ file)
    Left errs -> False <$ putStrLn ("FAIL " ++ file ++ ": expected " ++ show needle ++ ", got:\n" ++ unlines errs)
    Right _ -> False <$ putStrLn ("FAIL " ++ file ++ " was accepted")
