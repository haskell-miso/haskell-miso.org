# haskell-miso.org
#
# Everything runs through the flake, so a plain `make` works from a clean
# checkout with only nix installed.
#
#   make            update + build (WASM) + prerender + optimise  →  public/
#   make serve      serve public/ on http://localhost:8080
#   make js         build with the GHC JavaScript backend instead of WASM
#   make watch      hot reload via the WASM browser GHCi (ghciwatch)
#
# PROJECT selects the cabal project file:
#   cabal.project.dev (default) — build against the sibling ../miso checkout
#   cabal.project               — pin miso from github (requires a miso release
#                                 with misoWithContext / prerenderWithContext)

PROJECT ?= cabal.project.dev
CABAL_FLAGS = --project-file=$(PROJECT)

WASM_SHELL  = nix develop .\#wasm --command
GHC_SHELL   = nix develop .\#default --command
GHCJS_SHELL = nix develop .\#ghcjs --command

.PHONY: all update build prerender assets optim serve clean js watch repl

all: update build prerender optim

update:
	$(WASM_SHELL) wasm32-wasi-cabal $(CABAL_FLAGS) update

# Compile the client to WebAssembly and lay out public/
build:
	$(WASM_SHELL) bash -c '\
	  set -e; \
	  wasm32-wasi-cabal $(CABAL_FLAGS) build app; \
	  rm -rf public; mkdir -p public; \
	  cp -r static/. public/; \
	  cp -r assets public/assets; \
	  my_wasm=$$(wasm32-wasi-cabal $(CABAL_FLAGS) list-bin app | tail -n 1); \
	  $$(wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $$my_wasm --output public/ghc_wasm_jsffi.js; \
	  cp -v $$my_wasm public/app.wasm'

# Render every route to static HTML with vanilla GHC + miso -fssr
prerender:
	$(GHC_SHELL) bash -c 'set -e; mkdir -p public; cabal $(CABAL_FLAGS) run prerender'

# Shrink the WASM payload
optim:
	$(WASM_SHELL) bash -c '\
	  wasm-opt -all -O2 public/app.wasm -o public/app.wasm; \
	  wasm-tools strip -o public/app.wasm public/app.wasm'

serve:
	$(WASM_SHELL) http-server public -p 8080 -c-1

# Interactive development: WASM browser GHCi + ghciwatch on :8080
watch:
	$(WASM_SHELL) ghciwatch --after-startup-ghci :main --after-reload-ghci :main \
	  --watch shared --watch client --debounce 50ms \
	  --command 'wasm32-wasi-cabal $(CABAL_FLAGS) repl app --repl-options="-fghci-browser -fghci-browser-port=8080"'

repl:
	$(WASM_SHELL) wasm32-wasi-cabal $(CABAL_FLAGS) repl app --repl-options='-fghci-browser -fghci-browser-port=8080'

# JavaScript backend (GHC 9.12) instead of WASM
js:
	$(GHCJS_SHELL) bash -c '\
	  set -e; \
	  cabal $(CABAL_FLAGS) build app --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg; \
	  rm -rf public; mkdir -p public; \
	  cp -r static/. public/; \
	  cp -r assets public/assets; \
	  jsexe=$$(dirname $$(cabal $(CABAL_FLAGS) list-bin app --with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg)); \
	  cp -v $$jsexe/app.jsexe/all.js public/index.js'
	$(MAKE) prerender

clean:
	rm -rf dist-newstyle public
