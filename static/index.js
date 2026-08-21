// WASM loader for haskell-miso.org.
// Instantiates the GHC WebAssembly reactor with a WASI shim and calls the
// exported `hs_start`, which hydrates the prerendered page.
import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";

// Cache-busting: the prerendered HTML loads this module as
// /index.js?v=<build-hash>; propagate that stamp to the sibling files so
// the loader, FFI glue and wasm of one deploy always load together.
const v = new URL(import.meta.url).search;
const ghc_wasm_jsffi = (await import("./ghc_wasm_jsffi.js" + v)).default;

const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
];
const wasi = new WASI(args, env, fds, { debug: false });

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("/app.wasm" + v), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start();
