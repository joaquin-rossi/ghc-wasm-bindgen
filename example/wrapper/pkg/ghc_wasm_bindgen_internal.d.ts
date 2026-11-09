// ghc-wasm-bindgen runtime internals, imported by the generated bindings.

import type { Either } from "./ghc_wasm_bindgen_runtime";

/** Instantiates the .wasm module; must resolve before any binding is called. */
export function init(source: string | URL | BufferSource): Promise<void>;

/** A Haskell exception raised during a call. */
export class HaskellError extends Error {}

export function call(fn: string, ...args: unknown[]): any;

/** Opaque transfer representation. */
export type Value = { readonly __value: unique symbol };

export function pack(v: Value): string;
export function unpack(s: string): Value;
export function unpackBool(x: number): boolean;

/** Throws a TypeError for a value that is not a constructor of `type`. */
export function invalid(type: string, v: unknown): never;

// Standard types. Encoders throw a TypeError for invalid input.

export function encodeDouble(x: number): Value;
export function encodeFloat(x: number): Value;
/** Int and Word are 32 bits on wasm32. */
export function encodeInt(x: number): Value;
export function encodeWord(x: number): Value;
export function encodeInt8(x: number): Value;
export function encodeInt16(x: number): Value;
export function encodeInt32(x: number): Value;
/** Must be a safe integer. */
export function encodeInt64(x: number): Value;
export function encodeWord8(x: number): Value;
export function encodeWord16(x: number): Value;
export function encodeWord32(x: number): Value;
/** Must be a safe integer. */
export function encodeWord64(x: number): Value;
export function decodeNumber(v: Value | string): number;

export function encodeInteger(x: bigint): Value;
export function encodeNatural(x: bigint): Value;
export function decodeInteger(v: Value | string): bigint;

export function encodeBool(x: boolean): Value;
export function decodeBool(v: Value | string): boolean;

export function encodeString(x: string): Value;
/** Exactly one code point. */
export function encodeChar(x: string): Value;
export function decodeString(v: Value | string): string;

/** `YYYY-MM-DD`. */
export function encodeDay(x: string): Value;
export function encodeUTCTime(x: Date): Value;
export function decodeUTCTime(v: Value | string): Date;

export function encodeList<T>(xs: T[], f: (x: T) => Value): Value;
export function decodeList<T>(v: Value, f: (v: Value) => T): T[];

export function encodeNonEmpty<T>(xs: [T, ...T[]], f: (x: T) => Value): Value;

export function encodeSet<T>(xs: Set<T>, f: (x: T) => Value): Value;
export function decodeSet<T>(v: Value, f: (v: Value) => T): Set<T>;

/** Maps whose keys aeson writes as object keys (strings, numbers, Bool, Char, dates). */
export function encodeTextKeyMap<K, V>(m: Map<K, V>, key: (k: K) => Value, value: (x: V) => Value): Value;
export function decodeTextKeyMap<K, V>(v: Value, key: (k: string) => K, value: (v: Value) => V): Map<K, V>;

/** Maps written as arrays of pairs (any other key type, and IntMap). */
export function encodePairMap<K, V>(m: Map<K, V>, key: (k: K) => Value, value: (x: V) => Value): Value;
export function decodePairMap<K, V>(v: Value, key: (v: Value) => K, value: (v: Value) => V): Map<K, V>;

export function encodeMaybe<T>(x: T | null, f: (x: T) => Value): Value;
export function decodeMaybe<T>(v: Value, f: (v: Value) => T): T | null;

export function encodeEither<E, A>(x: Either<E, A>, left: (e: E) => Value, right: (a: A) => Value): Value;
export function decodeEither<E, A>(v: Value, left: (v: Value) => E, right: (v: Value) => A): Either<E, A>;

export function encodeTuple(xs: unknown[], fs: ((x: any) => Value)[]): Value;
export function decodeTuple(v: Value, fs: ((v: Value) => unknown)[]): any;

// User data types

export function encodeRecord(tag: string, fields: Record<string, Value>): Value;
export function encodePositional(tag: string, args: Value[]): Value;
/** For types whose constructors are all nullary. */
export function encodeEnum(tag: string): Value;

export function decodeTag(v: Value): unknown;
export function decodeField(v: Value, name: string): Value;
/** Field `i` of a positional constructor with `n` fields. */
export function decodeArg(v: Value, i: number, n: number): Value;
