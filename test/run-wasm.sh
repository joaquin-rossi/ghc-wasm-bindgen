#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

for tool in wasm32-wasi-ghc wasm32-wasi-cabal wasm-opt; do
    if ! command -v "$tool" >/dev/null; then
        echo "WASM integration tests not run: $tool is not on PATH (source ~/.ghc-wasm/env)." >&2
        exit 1
    fi
done

cabal build exe:ghc-wasm-bindgen
bindgen=$(cabal list-bin exe:ghc-wasm-bindgen)
(
    cd test/wasm
    "$bindgen" build OpaqueApi.hs --name opaque --out ../../dist-newstyle/opaque-integration
)
printf '{"type":"module"}\n' > dist-newstyle/opaque-integration/package.json
node test/wasm.mjs
