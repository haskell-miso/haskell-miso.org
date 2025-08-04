:ramen: haskell-miso.org
====================

The official [haskell-miso.org](https://haskell-miso.org) website.

See [live](https://haskell-miso.org)

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop --experimental-features nix-command --extra-experimental-features flakes
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

This will also build the prerendered HTML.

```bash
$ nix develop .#wasm --command bash -c "make" --experimental-features 'nix-command flakes'
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs --command bash -c "build" --experimental-features 'nix-command flakes'
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve" --experimental-features 'nix-command flakes'
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
