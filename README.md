<p align="center"> 
  <a href="https://haskell-miso.org"><img width="15%" height="512" alt="Image" src="https://github.com/user-attachments/assets/384a1898-0aed-4662-9a03-8dbe5641228f" /></a>
  <br />
  The <a href="https://haskell-miso.org">miso</a> 🍜 website.
</p>

## Features

- **Fully static.** A `prerender` executable (vanilla GHC + miso's `ssr` flag)
  renders every route to `public/<path>/index.html`. The same code compiles to
  WebAssembly and hydrates whichever page was loaded, then takes over
  navigation with `Miso.Router`.
- **One dependency.** The site depends on `base` and `miso` (plus the
  `directory` boot library in the generator). No servant, no aeson, no
  markdown — the docs are Haskell `View`s, the syntax highlighter is a small
  Haskell tokenizer, and JSON/SEO artifacts are printed by hand.
- **Everything is a component.** Each page is a keyed `Component` mounted with
  `(+>)`; the docs shell receives the current route as props; the language
  (with a translation table), and the theme live on the app-global context,
  persisted to `localStorage`.
- **Live docs.** Documentation pages embed running examples next to their
  source. `bun scripts/check-demos.ts` verifies the displayed source is
  exactly the code that executes (see `shared/Site/Demos.hs`).

## Layout

```
assets/logo/       the lambda logo (SVG + rendered PNGs, OG images)
client/Main.hs     WASM/JS entry point (misoWithContext + hydration)
server/Main.hs     static site generator (HTML, sitemap, RSS, manifest, motion.css)
shared/Site.hs     root component: top bar, router dispatch, footer
shared/Site/       pages, docs content, demos, i18n, search, styles
static/            style.css, index.js (WASM loader)
scripts/           check-demos.ts
```

## Building

Everything runs through the flake (only [nix](https://nixos.org) required):

```bash
make          # build WASM + prerender all pages into public/
make serve    # http-server public/  →  http://localhost:8080
make watch    # hot reload via the WASM browser GHCi
make js       # GHC JavaScript backend instead of WASM
```

To develop against a sibling `../miso` checkout instead of the pinned
release, pass `PROJECT=cabal.project.dev`:

```bash
make PROJECT=cabal.project.dev build prerender
```

## Serving

`public/` is a plain static site. Every route is a real directory with an
`index.html`, so any static file server works:

```bash
http-server public   # or: netlify / cloudflare pages / nginx / s3 …
```

Configure your host to serve `404.html` for unknown paths.
