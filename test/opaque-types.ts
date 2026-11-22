import * as api from "../dist-newstyle/test-bindings/opaque.js";
import { makeOther, makeSecret, readSecret, sameSecret, type Secret } from "../dist-newstyle/test-bindings/opaque.js";
import * as names from "../dist-newstyle/test-bindings/names.js";

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

const primed: names.Secret_ = names.makeSecret(1);
names.readSecret(primed);
names.sameSecret(primed).dispose();
primed.dispose();
const value: names.Value_ = names.Value_({ "field'": 1 });
names.echoValue(value);
names.echoValues([value, names.Empty_, names.Positional_(2)]);
const choice: names.Choice_ = names.First_;
names.echoChoice(choice);
names.echoChoices([choice, names.Second_]);
// @ts-expect-error Renaming identifiers must not rename the Haskell tag.
names.echoValue({ $tag: "Value_", "field'": 1 });
