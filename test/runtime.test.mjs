import assert from "node:assert/strict";
import { writeFile } from "node:fs/promises";
import { test } from "node:test";

// Run cabal test first: it generates these bindings from the real GHC model.
const bindings = new URL("../dist-newstyle/test-bindings/opaque.js", import.meta.url);
let importId = 0;
// load reads real bytes; the mocked instantiator supplies the test exports.
await writeFile(new URL("opaque.wasm", bindings), new Uint8Array());

function fakeWasm(setError) {
    const values = new Map();
    const free = [];
    const freed = [];
    let next = 1;
    let readsWith = 0;
    const retain = (value) => {
        const ptr = free.pop() ?? next++;
        values.set(ptr, value);
        return ptr;
    };
    const exports = {
        _initialize() {},
        bg_makeSecret: (n) => retain({ n }),
        bg_readSecret: (p) => {
            const value = values.get(p);
            assert.ok(value, "a freed pointer reached WASM");
            if (value.error) { setError("lazy secret failed"); return 0; }
            return value.n;
        },
        bg_sameSecret: (p) => retain(values.get(p)),
        bg_bumpSecret: (p, n) => retain({ n: values.get(p).n + n }),
        bg_readWith: (p, json) => {
            readsWith++;
            assert.ok(values.has(p), "a freed pointer reached WASM");
            return values.get(p).n + JSON.parse(json).reduce((a, b) => a + b, 0);
        },
        bg_makeOther: (n) => retain({ n }),
        bg_readOther: (p) => values.get(p).n,
        bg_failMake: () => { setError("cannot create secret"); return 0; },
        bg_failRead: () => { setError("cannot read secret"); return 0; },
        bg_lateFail: () => retain({ error: true }),
        bg_publicCount: (json) => JSON.parse(json).count,
        bg_echoPublic: (json) => json,
        bg_choose: (b) => JSON.stringify(b ? "First" : "Second"),
        ghc_wasm_bindgen_free(p) {
            assert.ok(values.delete(p), "a pointer was freed twice");
            freed.push(p);
            free.push(p);
            return 0;
        },
    };
    return { exports, values, freed, get readsWith() { return readsWith; } };
}

function setup(t, withFinalizers = true) {
    const registries = [];
    const instances = [];
    const original = globalThis.FinalizationRegistry;
    class Registry {
        entries = new Map();
        constructor(cleanup) { this.cleanup = cleanup; registries.push(this); }
        register(target, state, token) {
            this.entries.set(state, new WeakRef(token));
            assert.equal(target, token);
            assert.ok(!Object.values(state).includes(target), "finalizer held the wrapper alive");
        }
        unregister(token) {
            for (const [state, ref] of this.entries) {
                if (ref.deref() === token) return this.entries.delete(state);
            }
            return false;
        }
    }
    globalThis.FinalizationRegistry = withFinalizers ? Registry : undefined;
    t.after(() => { globalThis.FinalizationRegistry = original; });
    t.mock.method(WebAssembly, "instantiate", async (_, imports) => {
        const instance = fakeWasm(imports.ghc_wasm_jsffi.setError);
        instances.push(instance);
        return { instance: { exports: instance.exports } };
    });
    return {
        registries, instances,
        load: () => import(`${bindings.href}?test=${++importId}`),
    };
}

test("borrowed arguments remain usable and returned handles own independent references", async (t) => {
    const env = setup(t);
    const api = await env.load();
    const ref = api.makeSecret(12);
    assert.equal(api.readSecret(ref), 12);
    assert.equal(api.readSecret(ref), 12);
    const same = api.sameSecret(ref);
    const bumped = api.bumpSecret(ref, 3);
    assert.notEqual(same, ref);
    assert.deepEqual(Object.keys(ref), ["dispose"]);
    ref.dispose();
    assert.equal(api.readSecret(same), 12);
    assert.equal(api.readSecret(bumped), 15);
    same.dispose();
    bumped.dispose();
    assert.equal(env.instances[0].values.size, 0);
    assert.equal(env.instances[0].freed.length, 3);
});

test("wrong types, instances and forged references never reach WASM", async (t) => {
    const env = setup(t);
    const a = await env.load();
    const b = await env.load();
    const secret = a.makeSecret(1);
    const other = a.makeOther(2);
    const foreign = b.makeSecret(3);
    const hostile = new Proxy({}, { get() { throw new Error("must not inspect a forged reference"); } });
    for (const invalid of [other, foreign, 1, null, undefined, {}, { ...secret }, hostile]) {
        assert.throws(() => a.readSecret(invalid), TypeError);
    }
    assert.throws(() => a.readOther(secret), TypeError);
    assert.equal(a.readSecret(secret), 1);
    secret.dispose(); other.dispose(); foreign.dispose();
    assert.equal(env.instances[0].freed.length, 2);
    assert.equal(env.instances[1].freed.length, 1);
});

test("disposal is idempotent and stale wrappers stay invalid after pointer reuse", async (t) => {
    const env = setup(t);
    const api = await env.load();
    const ref = api.makeSecret(1);
    const alias = ref;
    const dispose = ref.dispose;
    dispose(); dispose(); alias.dispose();
    const replacement = api.makeSecret(2);
    assert.throws(() => api.readSecret(alias), TypeError);
    assert.equal(api.readSecret(replacement), 2);
    ref.dispose();
    assert.equal(api.readSecret(replacement), 2);
    replacement.dispose();
    assert.equal(env.instances[0].freed.length, 2);
});

test("explicit disposal unregisters cleanup and cleanup cannot double free", async (t) => {
    const env = setup(t);
    const api = await env.load();
    const ref = api.makeSecret(1);
    const registry = env.registries[0];
    const [state] = registry.entries.keys();
    ref.dispose();
    assert.equal(registry.entries.size, 0);
    registry.cleanup(state);
    registry.cleanup(state);
    assert.equal(env.instances[0].freed.length, 1);
});

test("finalizer cleanup releases through the owning instance", async (t) => {
    const env = setup(t);
    const a = await env.load();
    const ref = a.makeSecret(1);
    await env.load();
    const registry = env.registries[0];
    const [state] = registry.entries.keys();
    registry.cleanup(state);
    ref.dispose();
    assert.equal(env.instances[0].freed.length, 1);
    assert.equal(env.instances[1].freed.length, 0);
});

test("explicit ownership works without FinalizationRegistry", async (t) => {
    const env = setup(t, false);
    const api = await env.load();
    const ref = api.makeSecret(4);
    assert.equal(api.readSecret(ref), 4);
    ref.dispose();
    assert.equal(env.instances[0].values.size, 0);
});

test("wrapper creation failure releases its newly returned pointer", async (t) => {
    const env = setup(t);
    const api = await env.load();
    env.registries[0].register = () => { throw new Error("registration failed"); };
    assert.throws(() => api.makeSecret(1), /registration failed/);
    assert.equal(env.instances[0].values.size, 0);
    assert.equal(env.instances[0].freed.length, 1);
});

test("Haskell errors discard placeholders and subsequent calls still work", async (t) => {
    const env = setup(t);
    const api = await env.load();
    assert.throws(() => api.failMake(0), api.HaskellError);
    assert.equal(env.instances[0].values.size, 0);
    const ref = api.makeSecret(5);
    assert.throws(() => api.failRead(ref), api.HaskellError);
    assert.equal(api.readSecret(ref), 5);
    const lazy = api.lateFail(0);
    assert.throws(() => api.readSecret(lazy), /lazy secret failed/);
    assert.equal(api.readSecret(ref), 5);
    ref.dispose(); lazy.dispose();
});

test("getters cannot dispose a pointer after it has been borrowed", async (t) => {
    const env = setup(t);
    const api = await env.load();
    const ref = api.makeSecret(1);
    const values = [0];
    Object.defineProperty(values, 0, { get() { ref.dispose(); return 2; } });
    assert.throws(() => api.readWith(ref, values), TypeError);
    assert.equal(env.instances[0].readsWith, 0);
});

test("invalid value arguments preserve ownership and JSON/enum bindings still work", async (t) => {
    const env = setup(t);
    const api = await env.load();
    const ref = api.makeSecret(1);
    assert.throws(() => api.bumpSecret(ref, "bad"), TypeError);
    assert.equal(api.readWith(ref, [2, 3]), 6);
    ref.dispose();
    const value = api.Public({ count: 7, label: "hello" });
    assert.equal(api.publicCount(value), 7);
    assert.deepEqual(api.echoPublic(value), value);
    assert.equal(api.choose(true), api.First);
    assert.equal(api.choose(false), api.Second);
});
