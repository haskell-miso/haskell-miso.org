#!/bin/sh
# Installs the miso CLI. Usage: curl -fsSL https://haskell-miso.org/install.sh | sh
set -eu

repo="haskell-miso/miso-cli"
bin_name="miso"

os=$(uname -s)
arch=$(uname -m)

case "$os" in
  Linux)
    case "$arch" in
      x86_64) asset="miso-linux-x86_64" ;;
      aarch64|arm64) asset="miso-linux-aarch64" ;;
      *)
        echo "miso: unsupported Linux architecture: $arch" >&2
        exit 1
        ;;
    esac
    ;;
  Darwin)
    case "$arch" in
      arm64) asset="miso-macos-aarch64" ;;
      x86_64) asset="miso-macos-x86_64" ;;
      *)
        echo "miso: unsupported macOS architecture: $arch" >&2
        exit 1
        ;;
    esac
    ;;
  *)
    echo "miso: unsupported OS: $os" >&2
    exit 1
    ;;
esac

url="https://github.com/$repo/releases/latest/download/$asset"
install_dir="${MISO_INSTALL_DIR:-/usr/local/bin}"
dest="$install_dir/$bin_name"

echo "Downloading $asset..."
tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
curl -fsSL "$url" -o "$tmp"
chmod +x "$tmp"

if command -v nix >/dev/null 2>&1; then
  echo "note: Nix detected -- 'nix profile install github:$repo' avoids this script's use of $install_dir entirely"
fi

if mkdir -p "$install_dir" 2>/dev/null && [ -w "$install_dir" ]; then
  mv "$tmp" "$dest"
else
  echo "Installing to $dest requires sudo..."
  sudo mkdir -p "$install_dir"
  sudo mv "$tmp" "$dest"
fi

echo "Installed $bin_name to $dest"
"$dest" version

case ":$PATH:" in
  *":$install_dir:"*) ;;
  *)
    echo "warning: $install_dir is not on your PATH -- add it, or re-run with MISO_INSTALL_DIR set to a directory that is" >&2
    ;;
esac
