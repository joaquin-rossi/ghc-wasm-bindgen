// esbuild's file loader: importing a .wasm gives its URL.
declare module "*.wasm" {
    const url: string;
    export default url;
}
