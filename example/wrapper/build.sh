#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
source ~/.ghc-wasm/env

bindgen=$(cd ../.. && cabal -v0 build exe:ghc-wasm-bindgen && cabal -v0 list-bin exe:ghc-wasm-bindgen)
"$bindgen" build src/ShapeWasm.hs --name shape --out pkg
