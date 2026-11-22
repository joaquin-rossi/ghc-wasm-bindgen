# ghc-wasm-bindgen

Generate JavaScript/TypeScript bindings for a pure Haskell module compiled
with GHC's WASM backend.

```haskell
module ShapeWasm (Shape (..), area, scale, largest) where
import Shape
```

```ts
import { area, Circle, largest, Rect, scale } from "shape";

area(Rect({ width: 3, height: 4 }));  // 12
scale(2, Circle(1));                  // { $tag: "Circle", contents: 2 }
largest([]);                          // null
area(Circle("x"));                    // TypeScript error
```

See [`example/`](example/) for a complete project.

## Types

| Haskell                                         | TypeScript                    |
| ----------------------------------------------- | ----------------------------- |
| `Double`, `Float`                               | `number`                      |
| `Int`, `Word`, `Int8`…`Int64`, `Word8`…`Word64` | `number`                      |
| `Integer`, `Natural`                            | `bigint`                      |
| `Bool`                                          | `boolean`                     |
| `Char`, `String`, `Text`                        | `string`                      |
| `Day`                                           | `string` (YYYY-MM-DD)         |
| `UTCTime`                                       | `Date`                        |
| `Maybe a`                                       | `a \| null`                   |
| `Either e a`                                    | `{ Left: e } \| { Right: a }` |
| `(a, b)`                                        | `[a, b]`                      |
| `[a]`, `Vector a`                               | `a[]`                         |
| `NonEmpty a`                                    | `[a, ...a[]]`                 |
| `Set a`, `HashSet a`, `IntSet`                  | `Set<a>`                      |
| `Map k v`, `HashMap k v`, `IntMap v`            | `Map<k, v>`                   |

Your own `data` and `newtype` declarations:

| Constructors              | TypeScript                   |
| ------------------------- | ---------------------------- |
| with record fields        | `{ $tag: "C"; field: … }`    |
| positional                | `{ $tag: "C"; contents: … }` |
| all without fields (enum) | `"A" \| "B"`                 |

Notes:

- Every type the API uses must be exported. Export all its constructors for
  value conversion, or none for an opaque reference (see below). Exporting
  only some constructors is not supported.
- Not supported yet: `IO`, polymorphic functions, functions as arguments,
  operators, types with parameters and GADTs.
- In JavaScript/TypeScript identifiers, primes become underscores
  (`Secret'` → `Secret_`) and reserved words gain a trailing underscore.
  Constructor tags and record field keys keep their Haskell names. Name
  collisions after conversion are rejected.

## Opaque references

Export a type without its constructors to keep its values in Haskell:

```haskell
module ShapeWasm (Shape, circle, area, scale) where
import Shape (Shape (..), area, scale)

circle :: Double -> Shape
circle = Circle
```

The generated TypeScript `Shape` is an opaque type with a `dispose()` method.
Its contents and numeric handle are not exposed, and it has no JavaScript
constructor:

```ts
const shape = circle(3);
try {
    console.log(area(shape));
    const larger = scale(2, shape);
    try {
        console.log(area(larger));
    } finally {
        larger.dispose();
    }
} finally {
    shape.dispose();
}
```

- Each returned handle retains a Haskell value using `StablePtr`. Arguments
  borrow their handles: calling a function does not consume them.
- Each return creates an independently owned handle, even when the function
  returns an input value. Assigning a handle to another JS variable shares
  that same handle instead.
- `dispose()` releases the handle's retention; Haskell's GC reclaims the
  value once it has no other references. Repeated disposal has no effect.
- Using a disposed handle, a different opaque type, or a handle from another
  WASM instance throws `TypeError` before calling Haskell.
- When available, `FinalizationRegistry` also releases forgotten handles.
  Cleanup timing and execution are not guaranteed; use `dispose()` for
  predictable release.
- Opaque references currently work only as direct function arguments and
  results. Containers such as `[Shape]` and `Maybe Shape`, and fields of
  value-converted data types, cannot contain them. Opaque representations
  themselves are not inspected and may contain functions or private types.

Returned opaque values are evaluated to weak head normal form before being
retained. Exceptions in deeper, lazy contents surface when a subsequent
exported function evaluates those contents, as `HaskellError`.

For contributors, `cabal test` generates the bindings used by
`node --test test/runtime.test.mjs`. With the WASM toolchain on `PATH`, run
`bash test/run-wasm.sh` for the integration tests against compiled Haskell.

## Errors

- An argument that does not fit its type (an out-of-range integer, a `Char`
  that is not one code point, an unknown constructor, …) throws a `TypeError`
  before reaching Haskell.
- An exception in Haskell throws a `HaskellError` (exported by the bindings).

## Usage

### `build`: cabal projects

From the cabal project that contains the module:

```sh
ghc-wasm-bindgen build src/ShapeWasm.hs --name shape --out pkg/src
```

The module must be exposed by a library, in the same package as the code it
wraps or in one that depends on it. This writes `shape.js`, `shape.d.ts`,
`shape.wasm` and the JS runtime to `pkg/src/`, and nothing else, so the
directory around it can be maintained by hand, even in another repository.

To publish it as an npm package, write its `package.json` yourself, for
example:

```json
{
    "name": "shape",
    "version": "0.1.0",
    "type": "module",
    "exports": {
        ".": { "types": "./src/shape.d.ts", "default": "./src/shape.js" }
    }
}
```

### `generate`: plain source files

```sh
ghc-wasm-bindgen generate api/ShapeWasm.hs -i lib --name shape --hs-out gen --js-out out
```

This writes the Haskell bindings and runtime to `gen/`, and the JS bindings
and runtime to `out/`. Compiling is up to you:

```sh
wasm32-wasi-cabal install --lib aeson text --package-env .   # once
wasm32-wasi-ghc -package ghc-experimental -igen -ilib -iapi gen/ShapeWasmBindings.hs \
    -no-hs-main -optl-mexec-model=reactor -outputdir build -o shape.wasm
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i shape.wasm -o out/ghc_wasm_jsffi.js
wasm-opt -Oz shape.wasm -o out/shape.wasm
```

`shape.wasm` must end up next to `shape.js`.

## Loading

`shape.js` loads `shape.wasm` from its own directory with a top-level
`await`, so importing it is enough: the functions are ready to call. If the
`.wasm` cannot be loaded, the import fails.

This needs ES2022 modules (top-level `await`): current browsers, Node 20.16+
or 22.3+, and bundlers targeting ES2022 or later. The `.wasm` is referenced as
`new URL("./shape.wasm", import.meta.url)`:

- webpack 5, Vite and Parcel copy it to the output on their own. With Vite,
  exclude the package from dependency pre-bundling
  (`optimizeDeps: { exclude: ["shape"] }`), which would break that reference
  in development.
- esbuild leaves the reference as is: copy `shape.wasm` next to the bundle
  (the [example](example/) does this).

## Installation

```sh
cabal install ghc-wasm-bindgen
```

Requirements:

- GHC 9.10 and cabal.
- The WASM toolchain from
  [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta) on
  `PATH` (tested with GHC 9.14):

  ```sh
  curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.14 sh
  ```
