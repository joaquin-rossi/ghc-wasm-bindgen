import * as api from "../dist-newstyle/test-bindings/opaque.js";
import { makeOther, makeSecret, readSecret, sameSecret, type Secret } from "../dist-newstyle/test-bindings/opaque.js";

const ref: Secret = makeSecret(1);
readSecret(ref);
sameSecret(ref).dispose();
ref.dispose();

// @ts-expect-error Different opaque types must not be interchangeable.
readSecret(makeOther(1));
// @ts-expect-error The brand cannot be manufactured by providing dispose.
readSecret({ dispose() {} });
// @ts-expect-error Handles are not exposed as numbers.
readSecret(1);
// @ts-expect-error A normal constructor is not exported for an abstract type.
new api.Secret();
