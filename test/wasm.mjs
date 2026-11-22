import assert from "node:assert/strict";
import * as api from "../dist-newstyle/opaque-integration/opaque.js";

const ref = api.makeSecret(12);
const same = api.sameSecret(ref);
const bumped = api.bumpSecret(ref, 3);
assert.equal(api.readSecret(ref), 12);
assert.equal(api.readWith(ref, [1, 2]), 15);
ref.dispose();
ref.dispose();
assert.throws(() => api.readSecret(ref), TypeError);
assert.equal(api.readSecret(same), 12);
assert.equal(api.readSecret(bumped), 15);

const other = api.makeOther(2);
assert.throws(() => api.readSecret(other), TypeError);
assert.equal(api.readOther(other), 2);
assert.throws(() => api.failMake(0), api.HaskellError);
assert.throws(() => api.failRead(same), api.HaskellError);
const lazy = api.lateFail(0);
assert.throws(() => api.readSecret(lazy), api.HaskellError);
assert.equal(api.readSecret(same), 12);

const value = api.Public({ count: 7, label: "hello" });
assert.deepEqual(api.echoPublic(value), value);
assert.equal(api.publicCount(value), 7);
assert.equal(api.choose(true), api.First);
for (const handle of [same, bumped, other, lazy]) handle.dispose();
console.log("ok   WASM opaque references, exceptions and value conversions");
