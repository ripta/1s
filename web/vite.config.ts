import { defineConfig } from "vite";
import wasmPack from "vite-plugin-wasm-pack";

export default defineConfig({
  base: "/one-stack-web/dist/",
  build: {
    minify: false,
  },
  plugins: [
    // wasmPack(["../"]),
  ],
});
