// Types for the output of GHC's post-link.mjs.

declare const jsffiImports: (exports: Record<string, unknown>) => WebAssembly.ModuleImports;
export default jsffiImports;
