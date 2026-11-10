// Public interface of the ghc-wasm-bindgen runtime.

export { init, HaskellError } from "./ghc_wasm_bindgen_internal";

export type Either<E, A> = { Left: E } | { Right: A };
