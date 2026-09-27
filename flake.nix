{
  description = "haskell-miso.org — the miso website, written in miso";

  inputs = {
    miso.url = "github:dmjio/miso";
    # The MicroHs backend lives on miso's rts_standalone branch until it is merged.
    miso-mhs.url = "github:dmjio/miso/rts_standalone";
  };

  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system: {
      # `nix develop`         → vanilla GHC 9.12 (prerenderer)
      # `nix develop .#wasm`  → GHC WASM backend (the site itself)
      # `nix develop .#ghcjs` → GHC JavaScript backend
      # `nix develop .#mhs`   → MicroHs (mhs) with miso installed, and emscripten
      devShell = inputs.miso.outputs.devShells.${system}.default;
      devShells.default = inputs.miso.outputs.devShells.${system}.default;
      devShells.wasm = inputs.miso.outputs.devShells.${system}.wasm;
      devShells.ghcjs = inputs.miso.outputs.devShells.${system}.ghcjs;
      devShells.mhs = inputs.miso-mhs.outputs.devShells.${system}.mhs;
    });
}
