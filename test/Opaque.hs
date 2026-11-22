module Opaque (tests) where

import qualified Data.Text as Text
import qualified Data.Text.IO as T
import System.Directory (copyFile, createDirectoryIfMissing)

import GhcWasmBindgen.EmitHaskell (emitHaskell)
import GhcWasmBindgen.EmitJavaScript (emitDts, emitJs)
import GhcWasmBindgen.Inspect (InspectOptions (..), Source (..), inspect)
import GhcWasmBindgen.Model

tests :: IO Bool
tests = and <$> sequence
  [ testApi "OpaqueApi" "opaque" $ \api ->
      [ ("abstract types", apiOpaqueTypes api == ["Other", "Secret"])
      , ("value types", map adtName (apiTypes api) == ["Choice", "Public"])
      , ("reference arguments and results", Fn "sameSecret" [TOpaque "Secret"] (TOpaque "Secret") `elem` apiFns api)
      , ("mixed arguments", Fn "bumpSecret" [TOpaque "Secret", TInt] (TOpaque "Secret") `elem` apiFns api)
      ]
  , testApi "NamesApi" "names" $ \api ->
      [ ("original opaque names", apiOpaqueTypes api == ["Secret'"])
      , ("original value names", map adtName (apiTypes api) == ["Choice'", "Value'"])
      , ("original reference types", Fn "sameSecret" [TOpaque "Secret'"] (TOpaque "Secret'") `elem` apiFns api)
      , ("original nested types", Fn "echoValues" [TList (TAdt "Value'")] (TList (TAdt "Value'")) `elem` apiFns api)
      ]
  ]

testApi :: String -> String -> (Api -> [(String, Bool)]) -> IO Bool
testApi modName name checksFor = do
  result <- inspect (InspectOptions ("test/wasm/" ++ modName ++ ".hs") (SourceFiles []))
  case result of
    Left errs -> False <$ putStrLn ("FAIL " ++ modName ++ ": " ++ unlines errs)
    Right api -> do
      let checks = checksFor api
          out = "dist-newstyle/test-bindings/"
      createDirectoryIfMissing True out
      T.writeFile (out ++ name ++ ".js") (emitJs (Text.pack name) api)
      T.writeFile (out ++ name ++ ".d.ts") (emitDts api)
      T.writeFile (out ++ modName ++ "Bindings.hs") (emitHaskell api)
      T.writeFile (out ++ "package.json") "{\"type\":\"module\"}\n"
      -- Node tests provide WASM exports through WebAssembly.instantiate.
      T.writeFile (out ++ "ghc_wasm_jsffi.js") "export default exports => ({ setError: message => { exports.__ghcWasmBindgenError = message; } });\n"
      copyFile "runtime-js/ghc_wasm_bindgen_internal.js" (out ++ "ghc_wasm_bindgen_internal.js")
      copyFile "runtime-js/ghc_wasm_bindgen_internal.d.ts" (out ++ "ghc_wasm_bindgen_internal.d.ts")
      mapM_ (\(label, ok) -> putStrLn ((if ok then "ok   " else "FAIL ") ++ label)) checks
      pure (all snd checks)
