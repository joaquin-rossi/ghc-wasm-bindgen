import { area, Circle, isSquare, largest, Rect, scale, totalArea, type Shape } from "shape";
import { init } from "shape/runtime";
import wasm from "shape/shape.wasm";

const results = document.querySelector<HTMLTableSectionElement>("#results")!;
const summary = document.querySelector<HTMLParagraphElement>("#summary")!;
let passed = 0;
let total = 0;

function check(code: string, expected: string, call: () => unknown) {
    let got: string;
    try {
        got = JSON.stringify(call());
    } catch (e) {
        got = (e as Error).name;
    }
    const ok = got === expected;
    total++;
    if (ok) passed++;
    const row = results.insertRow();
    row.className = ok ? "pass" : "fail";
    for (const text of [ok ? "✓" : "✗", code, expected, got]) {
        row.insertCell().textContent = text;
    }
}

await init(new URL(wasm, import.meta.url));

check(
    "area(Rect({ width: 3, height: 4 }))",
    "12",
    () => area(Rect({ width: 3, height: 4 })),
);

check(
    "area(Circle(1))",
    String(Math.PI),
    () => area(Circle(1)),
);

check(
    "scale(2, Circle(1))",
    '{"$tag":"Circle","contents":2}',
    () => scale(2, Circle(1)),
);

check(
    "scale(0.5, Rect({ width: 3, height: 4 }))",
    '{"$tag":"Rect","width":1.5,"height":2}',
    () => scale(0.5, Rect({ width: 3, height: 4 })),
);

check(
    "isSquare(Rect({ width: 2, height: 2 }))",
    "true",
    () => isSquare(Rect({ width: 2, height: 2 })),
);

check(
    "isSquare(Circle(1))",
    "false",
    () => isSquare(Circle(1)),
);

check(
    "totalArea([Rect({ width: 2, height: 3 }), Rect({ width: 1, height: 1 })])",
    "7",
    () => totalArea([Rect({ width: 2, height: 3 }), Rect({ width: 1, height: 1 })]),
);

check(
    "largest([Circle(1), Rect({ width: 1, height: 1 })])",
    '{"$tag":"Circle","contents":1}',
    () => largest([Circle(1), Rect({ width: 1, height: 1 })]),
);

check(
    "largest([])",
    "null",
    () => largest([]),
);

check(
    'area({ $tag: "Hexagon" })',
    "TypeError",
    () => area({ $tag: "Hexagon" } as unknown as Shape),
);

summary.textContent = `${passed}/${total} checks passed`;
