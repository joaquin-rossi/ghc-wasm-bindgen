// ghc-wasm-bindgen runtime internals, imported by the generated bindings.

import jsffiImports from "./ghc_wasm_jsffi.js";

let wasm;
let ready;

export function init(source) {
    return (ready ??= load(source));
}

async function load(source) {
    if (typeof source === "string" || source instanceof URL) {
        const res = await fetch(source);
        if (!res.ok) throw new Error(`ghc-wasm-bindgen: fetching ${source} failed (${res.status})`);
        source = await res.arrayBuffer();
    }
    // jsffiImports needs the exports, which exist only after instantiation.
    const knot = {};
    const { instance } = await WebAssembly.instantiate(source, {
        ghc_wasm_jsffi: jsffiImports(knot),
        wasi_snapshot_preview1: wasi(knot),
    });
    Object.assign(knot, instance.exports);
    knot._initialize();
    wasm = knot;
}

// WASI subset the RTS uses for pure code; other calls return ENOSYS.
function wasi(exports) {
    const view = () => new DataView(exports.memory.buffer);
    const noArgs = (count, size) => {
        view().setUint32(count, 0, true);
        view().setUint32(size, 0, true);
        return 0;
    };
    const calls = {
        args_sizes_get: noArgs,
        environ_sizes_get: noArgs,
        args_get: () => 0,
        environ_get: () => 0,
        clock_time_get: (clock, _precision, out) => {
            const ms = clock === 0 ? Date.now() : performance.now();
            view().setBigUint64(out, BigInt(Math.round(ms * 1e6)), true);
            return 0;
        },
        fd_write: (fd, iovs, count, written) => {
            const v = view();
            let text = "";
            let total = 0;
            for (let i = 0; i < count; i++) {
                const ptr = v.getUint32(iovs + 8 * i, true);
                const len = v.getUint32(iovs + 8 * i + 4, true);
                text += new TextDecoder().decode(new Uint8Array(exports.memory.buffer, ptr, len));
                total += len;
            }
            v.setUint32(written, total, true);
            (fd === 2 ? console.warn : console.log)(text.replace(/\n$/, ""));
            return 0;
        },
        proc_exit: (code) => {
            throw new Error(`ghc-wasm-bindgen: the Haskell runtime exited (code ${code})`);
        },
    };
    const ENOSYS = 52;
    return new Proxy(calls, { get: (target, name) => target[name] ?? (() => ENOSYS) });
}

export class HaskellError extends Error {
    name = "HaskellError";
}

// __ghcWasmBindgenError is set by GhcWasmBindgen.Runtime.guarded.
export function call(fn, ...args) {
    if (!wasm) throw new Error("ghc-wasm-bindgen: await init() before calling the API");
    const result = wasm[fn](...args);
    const err = wasm.__ghcWasmBindgenError;
    if (err !== undefined) {
        wasm.__ghcWasmBindgenError = undefined;
        throw new HaskellError(err);
    }
    return result;
}

// Values: aeson JSON, with the options in GhcWasmBindgen.Runtime. Encoders
// check their input; decoders trust what Haskell returns.

// bigints are written as JSON numbers, and integers beyond 2^53 are read as
// bigints, so Integer and Natural keep their precision.
export function pack(v) {
    const tag = marker();
    const json = JSON.stringify(v, (_, x) => (typeof x === "bigint" ? tag + x : x));
    return json.replace(new RegExp(`"\\\\u0000${tag.slice(1)}(-?\\d+)"`, "g"), "$1");
}

export function unpack(s) {
    const tag = marker();
    const text = s.replace(/"(?:\\.|[^"\\])*"|-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/g, (token) =>
        /^-?\d+$/.test(token) && !Number.isSafeInteger(Number(token)) ? JSON.stringify(tag + token) : token,
    );
    return JSON.parse(text, (_, x) => (typeof x === "string" && x.startsWith(tag) ? BigInt(x.slice(tag.length)) : x));
}

const marker = () => `\u0000${Math.random().toString(36).slice(2)}:`;

export const unpackBool = (x) => x !== 0; // JSFFI returns Bool as 0/1

export function invalid(type, v) {
    let shown;
    try {
        shown = JSON.stringify(v, (_, x) => (typeof x === "bigint" ? `${x}n` : x));
    } catch {
        shown = String(v);
    }
    throw new TypeError(`ghc-wasm-bindgen: not a valid ${type}: ${shown}`);
}

const check = (type, ok) => (x) => (ok(x) ? x : invalid(type, x));
const integral = (type, min, max) => check(type, (x) => Number.isInteger(x) && x >= min && x <= max);

// Numbers. Int and Word are 32 bits on wasm32; 64-bit types must be safe integers.
export const encodeDouble = check("Double", (x) => typeof x === "number");
export const encodeFloat = check("Float", (x) => typeof x === "number");
export const encodeInt = integral("Int", -(2 ** 31), 2 ** 31 - 1);
export const encodeWord = integral("Word", 0, 2 ** 32 - 1);
export const encodeInt8 = integral("Int8", -(2 ** 7), 2 ** 7 - 1);
export const encodeInt16 = integral("Int16", -(2 ** 15), 2 ** 15 - 1);
export const encodeInt32 = integral("Int32", -(2 ** 31), 2 ** 31 - 1);
export const encodeInt64 = integral("Int64", Number.MIN_SAFE_INTEGER, Number.MAX_SAFE_INTEGER);
export const encodeWord8 = integral("Word8", 0, 2 ** 8 - 1);
export const encodeWord16 = integral("Word16", 0, 2 ** 16 - 1);
export const encodeWord32 = integral("Word32", 0, 2 ** 32 - 1);
export const encodeWord64 = integral("Word64", 0, Number.MAX_SAFE_INTEGER);
export const decodeNumber = (v) => Number(v);

export const encodeInteger = check("Integer", (x) => typeof x === "bigint");
export const encodeNatural = check("Natural", (x) => typeof x === "bigint" && x >= 0n);
export const decodeInteger = (v) => BigInt(v);

export const encodeBool = check("Bool", (x) => typeof x === "boolean");
export const decodeBool = (v) => v === true || v === "true";

export const encodeString = check("String", (x) => typeof x === "string");
export const encodeChar = check("Char", (x) => typeof x === "string" && [...x].length === 1);
export const decodeString = (v) => v;

export const encodeDay = check("Day", (x) => typeof x === "string" && /^-?\d{4,}-\d{2}-\d{2}$/.test(x));
export const encodeUTCTime = (x) =>
    x instanceof Date && !Number.isNaN(x.getTime()) ? x.toISOString() : invalid("UTCTime", x);
export const decodeUTCTime = (v) => new Date(v);

// Containers.
export const encodeList = (xs, f) => (Array.isArray(xs) ? xs.map((x) => f(x)) : invalid("list", xs));
export const decodeList = (v, f) => v.map((x) => f(x));

export const encodeNonEmpty = (xs, f) =>
    Array.isArray(xs) && xs.length > 0 ? xs.map((x) => f(x)) : invalid("NonEmpty", xs);

export const encodeSet = (xs, f) => (xs instanceof Set ? [...xs].map((x) => f(x)) : invalid("Set", xs));
export const decodeSet = (v, f) => new Set(v.map((x) => f(x)));

// Maps with string, number, Bool, Char or date keys are objects; any other
// key type makes them arrays of pairs.
export const encodeTextKeyMap = (m, key, value) =>
    m instanceof Map
        ? Object.fromEntries([...m].map(([k, x]) => [String(key(k)), value(x)]))
        : invalid("Map", m);
export const decodeTextKeyMap = (v, key, value) => new Map(Object.entries(v).map(([k, x]) => [key(k), value(x)]));

export const encodePairMap = (m, key, value) =>
    m instanceof Map ? [...m].map(([k, x]) => [key(k), value(x)]) : invalid("Map", m);
export const decodePairMap = (v, key, value) => new Map(v.map(([k, x]) => [key(k), value(x)]));

export const encodeMaybe = (x, f) => (x === null ? null : f(x));
export const decodeMaybe = (v, f) => (v === null ? null : f(v));

export const encodeEither = (x, left, right) =>
    x !== null && typeof x === "object" && "Left" in x
        ? { Left: left(x.Left) }
        : x !== null && typeof x === "object" && "Right" in x
          ? { Right: right(x.Right) }
          : invalid("Either", x);
export const decodeEither = (v, left, right) => ("Left" in v ? { Left: left(v.Left) } : { Right: right(v.Right) });

export const encodeTuple = (xs, fs) =>
    Array.isArray(xs) && xs.length === fs.length ? fs.map((f, i) => f(xs[i])) : invalid(`${fs.length}-tuple`, xs);
export const decodeTuple = (v, fs) => fs.map((f, i) => f(v[i]));

// User data types.
export const encodeRecord = (tag, fields) => ({ $tag: tag, ...fields });
export const encodePositional = (tag, args) =>
    args.length === 0 ? { $tag: tag } : { $tag: tag, contents: args.length === 1 ? args[0] : args };
export const encodeEnum = (tag) => tag;

export const decodeTag = (v) => (typeof v === "string" ? v : v?.$tag);
export const decodeField = (v, name) => v[name];
export const decodeArg = (v, i, n) => (n === 1 ? v.contents : v.contents[i]);
