module Opaque (tests) where

import qualified Data.Text.IO as T
import System.Directory (copyFile, createDirectoryIfMissing)

import GhcWasmBindgen.EmitHaskell (emitHaskell)
import GhcWasmBindgen.EmitJavaScript (emitDts, emitJs)
import GhcWasmBindgen.Inspect (InspectOptions (..), Source (..), inspect)
import GhcWasmBindgen.Model

tests :: IO Bool
tests = do
  result <- inspect (InspectOptions "test/wasm/OpaqueApi.hs" (SourceFiles []))
  case result of
    Left errs -> False <$ putStrLn ("FAIL opaque API: " ++ unlines errs)
    Right api -> do
      let checks =
            [ ("abstract types", apiOpaqueTypes api == ["Other", "Secret"])
            , ("value types", map adtName (apiTypes api) == ["Choice", "Public"])
            , ("reference arguments and results", Fn "sameSecret" [TOpaque "Secret"] (TOpaque "Secret") `elem` apiFns api)
            , ("mixed arguments", Fn "bumpSecret" [TOpaque "Secret", TInt] (TOpaque "Secret") `elem` apiFns api)
            ]
          out = "dist-newstyle/test-bindings/"
      createDirectoryIfMissing True out
      T.writeFile (out ++ "opaque.js") (emitJs "opaque" api)
      T.writeFile (out ++ "opaque.d.ts") (emitDts api)
      T.writeFile (out ++ "OpaqueBindings.hs") (emitHaskell api)
      T.writeFile (out ++ "package.json") "{\"type\":\"module\"}\n"
      -- Node tests provide WASM exports through WebAssembly.instantiate.
      T.writeFile (out ++ "ghc_wasm_jsffi.js") "export default exports => ({ setError: message => { exports.__ghcWasmBindgenError = message; } });\n"
      copyFile "runtime-js/ghc_wasm_bindgen_internal.js" (out ++ "ghc_wasm_bindgen_internal.js")
      copyFile "runtime-js/ghc_wasm_bindgen_internal.d.ts" (out ++ "ghc_wasm_bindgen_internal.d.ts")
      mapM_ (\(name, ok) -> putStrLn ((if ok then "ok   " else "FAIL ") ++ name)) checks
      pure (all snd checks)
