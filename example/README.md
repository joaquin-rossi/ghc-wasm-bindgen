# Example: Shape

A pure Haskell library exposed to a web page.

```
haskell/                  the library (Shape.hs)
wrapper/src/ShapeWasm.hs  the API exposed to JS (its export list)
wrapper/pkg/package.json  npm package, written by hand
wrapper/pkg/src/          generated (shape.js, shape.wasm, …), committed
web/src/main.ts           the page, depends on the package
```

## Run

```sh
cd example/web
npm install
npm start    # http://127.0.0.1:8000/
```

`wrapper/pkg/src/` is committed (its `.wasm` through Git LFS), so this needs no
Haskell toolchain.

## Rebuild after changing the Haskell code

```sh
example/wrapper/build.sh
```

Requires the WASM toolchain in `~/.ghc-wasm` (see the main README).
