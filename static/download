#!/bin/sh
# Install the miso tool:
#
#   curl -fsSL https://haskell-miso.org/download | sh
#
# Downloads the release for this OS and architecture from GitHub, checks its
# sha256, unpacks it in $MISO_HOME (default ~/.miso), and offers to add
# $MISO_HOME/bin to PATH in your shell's startup file.  On NixOS it says
# where to get miso with nix instead.
#
# Options (curl -fsSL https://haskell-miso.org/download | sh -s -- --yes):
#   --yes              do not ask, take the defaults (on NixOS: install in
#                      $MISO_HOME)
#   --no-modify-path   do not edit a shell startup file
#
# MISO_VERSION installs a release (default: the latest), MISO_HOME installs
# somewhere else, and MISO_RELEASE_URL downloads from a mirror (or file://).
# This file is served at https://haskell-miso.org/download, by the website
# (haskell-miso/haskell-miso.org); miso upgrade runs it too.
set -eu

repo=${MISO_REPO:-haskell-miso/cli}
home=${MISO_HOME:-$HOME/.miso}
version=${MISO_VERSION:-latest}

yes=
modify_path=1
for arg in "$@"; do
  case "$arg" in
    -y|--yes) yes=1 ;;
    --no-modify-path) modify_path= ;;
    *) printf 'usage: install.sh [--yes] [--no-modify-path]\n' >&2; exit 1 ;;
  esac
done

if [ -t 1 ] && [ -z "${NO_COLOR:-}" ]; then
  esc=$(printf '\033')
  bold="${esc}[1m"; dim="${esc}[2m"; red="${esc}[31m"; green="${esc}[32m"; yellow="${esc}[33m"
  blue="${esc}[34m"; magenta="${esc}[35m"; cyan="${esc}[36m"; reset="${esc}[0m"
else
  bold=; dim=; red=; green=; yellow=; blue=; magenta=; cyan=; reset=
fi
say()   { printf '%s==>%s %s\n' "$bold$blue" "$reset" "$1"; }
done_() { printf '%s==>%s %s\n' "$bold$green" "$reset" "$1"; }
note()  { printf '%s==>%s %s\n' "$bold$yellow" "$reset" "$1"; }
die()   { printf '%serror:%s %s\n' "$bold$red" "$reset" "$1" >&2; exit 1; }

# Questions are read from the terminal, as curl ... | sh gives the script on
# stdin.  Without a terminal (CI), or with --yes, the default is taken.
tty=
if [ -z "$yes" ] && (: < /dev/tty) 2>/dev/null; then tty=1; fi
# ask QUESTION DEFAULT: is the answer yes?  DEFAULT is y or n.
ask() {
  if [ -z "$tty" ]; then [ "$2" = y ]; return; fi
  if [ "$2" = y ]; then hint="[Y/n]"; else hint="[y/N]"; fi
  printf '%s?%s %s %s ' "$bold$cyan" "$reset" "$1" "$hint" > /dev/tty
  read -r answer < /dev/tty || answer=
  case "$answer" in
    [Yy]|[Yy][Ee][Ss]) return 0 ;;
    [Nn]|[Nn][Oo]) return 1 ;;
    *) [ "$2" = y ] ;;
  esac
}

case "$(uname -s)" in
  Linux)  os=linux ;;
  Darwin) os=darwin ;;
  MINGW*|MSYS*|CYGWIN*) os=windows ;;     # Windows: run this in an MSYS2 shell
  *) die "unsupported OS: $(uname -s) (miso supports Linux, macOS, and Windows with MSYS2)" ;;
esac
case "$(uname -m)" in
  x86_64|amd64)  arch=x86_64 ;;
  arm64|aarch64) arch=aarch64 ;;
  *) die "unsupported architecture: $(uname -m)" ;;
esac
# an Intel shell under Rosetta on Apple Silicon gets the native release
if [ "$os" = darwin ] && [ "$arch" = x86_64 ] && [ "$(sysctl -n sysctl.proc_translated 2>/dev/null || true)" = 1 ]; then
  arch=aarch64
fi
system=$arch-$os
[ "$system" = aarch64-windows ] && die "there is no release for Windows on ARM yet"
asset=miso-$system.tar.gz

if [ -n "${MISO_RELEASE_URL:-}" ]; then
  url=$MISO_RELEASE_URL
elif [ "$version" = latest ]; then
  url=https://github.com/$repo/releases/latest/download
else
  url=https://github.com/$repo/releases/download/$version
fi

command -v curl >/dev/null || die "curl is needed"
if command -v sha256sum >/dev/null; then sha="sha256sum"
elif command -v shasum >/dev/null; then sha="shasum -a 256"
else die "sha256sum or shasum is needed"; fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

printf '\n%smiso%s: build miso apps with MicroHs (%s)\n\n' "$bold$magenta" "$reset" "$system"

#############################################################################
# NixOS: miso comes from its flake (#release: the release, from nix/release.json)

nixos=
if [ "$os" = linux ] && { [ -e /etc/NIXOS ] || grep -qs '^ID=nixos' /etc/os-release; }; then nixos=1; fi
# (--yes installs in $MISO_HOME anyway, as does a reinstall or miso upgrade)
if [ -n "$nixos" ] && [ -z "$yes" ] && [ ! -e "$home/bin/miso" ]; then
  note "This is NixOS: install miso with nix, from https://github.com/$repo"
  printf '\n    %snix profile install github:%s#release%s    %s# the release, prebuilt%s\n' \
    "$bold" "$repo" "$reset" "$dim" "$reset"
  printf '    %snix profile install github:%s%s            %s# or built from source%s\n' \
    "$bold" "$repo" "$reset" "$dim" "$reset"
  printf '\n  In a flake: the input github:%s, and its packages.<system>.release.\n' "$repo"
  printf '  (To install in %s anyway: curl -fsSL https://haskell-miso.org/download | sh -s -- --yes)\n' "$home"
  exit 0
fi

#############################################################################
# Install in $MISO_HOME

say "Downloading miso ($version) for $system"
curl -fSL --progress-bar -o "$tmp/$asset" "$url/$asset" || die "could not download $url/$asset"
curl -fsSL -o "$tmp/$asset.sha256" "$url/$asset.sha256" || die "could not download $url/$asset.sha256"
(cd "$tmp" && $sha -c "$asset.sha256" >/dev/null) || die "checksum mismatch for $asset"
sum=$(cut -c1-16 "$tmp/$asset.sha256")
printf '  %s✓%s sha256 %s... matches %s\n' "$bold$green" "$reset" "$sum" "$asset.sha256"

tar -C "$tmp" -xzf "$tmp/$asset"
new=$tmp/miso-$system
new_version=$("$new/bin/miso" version) || die "the downloaded miso does not run on this system"

# Paths as the user knows them: ~ for $HOME
tilde() {
  case "$1" in
    "$HOME"/*) printf '~/%s' "${1#"$HOME"/}" ;;
    *) printf '%s' "$1" ;;
  esac
}
size() { du -sh "$1" 2>/dev/null | cut -f1; }
# plan VERB PATH NOTE: a line of what the install does
plan() { printf '      %s%-8s%s %s%s\n' "$bold" "$1" "$reset" "$2" "${3:+  $dim($3)$reset}"; }

# What the install does, before it does it.  A new release replaces the tool
# and its package database (packages from miso install are installed again by
# the next miso build).
say "$new_version will be installed in $(tilde "$home"):"
for f in "$new"/bin/*; do
  b=$(basename "$f")
  if [ -e "$home/bin/$b" ]; then plan replace "$(tilde "$home/bin/$b")" "$(size "$f")"
  else plan create "$(tilde "$home/bin/$b")" "$(size "$f")"; fi
done
for d in "$new"/mhs-*; do
  b=$(basename "$d")
  if [ -e "$home/$b" ]; then plan replace "$(tilde "$home/$b")/" "the package database, $(size "$d")"
  else plan create "$(tilde "$home/$b")/" "the package database, $(size "$d")"; fi
done
for d in "$home"/mhs-*; do
  if [ -e "$d" ] && [ ! -e "$new/$(basename "$d")" ]; then
    plan remove "$(tilde "$d")/" "the package database of another version"
  fi
done
for f in "$home"/bin/*; do
  if [ -e "$f" ] && [ ! -e "$new/bin/$(basename "$f")" ]; then plan remove "$(tilde "$f")"; fi
done
if ! ask "Install miso?" y; then
  printf '\nNothing was changed.\n'
  exit 0
fi

mkdir -p "$home"
# (Windows cannot delete a running miso.exe, as during miso upgrade, but can rename it.)
[ -f "$home/bin/miso.exe" ] && mv -f "$home/bin/miso.exe" "$home/miso.exe.old" 2>/dev/null || true
rm -rf "${home:?}/bin" "$home"/mhs-* 2>/dev/null || true
cp -R "$new/." "$home/"
done_ "Installed $("$home/bin/miso" version) in $(tilde "$home"):"
for f in "$home"/bin/* "$home"/mhs-*; do
  case "$f" in */mhs-*) printf '      %-40s %s\n' "$(tilde "$f")/" "$(size "$f")" ;;
               *) printf '      %-40s %s\n' "$(tilde "$f")" "$(size "$f")" ;; esac
done

# PATH: the startup file of the user's shell
case "$home" in
  "$HOME"/*) shown="\$HOME/${home#"$HOME"/}" ;;
  *) shown=$home ;;
esac
case "$(basename "${SHELL:-sh}")" in
  zsh)  rc=${ZDOTDIR:-$HOME}/.zshrc; line="export PATH=\"$shown/bin:\$PATH\"" ;;
  bash) if [ "$os" = darwin ]; then rc=$HOME/.bash_profile; else rc=$HOME/.bashrc; fi
        line="export PATH=\"$shown/bin:\$PATH\"" ;;
  fish) rc=${XDG_CONFIG_HOME:-$HOME/.config}/fish/config.fish; line="fish_add_path \"$shown/bin\"" ;;
  *)    rc=$HOME/.profile; line="export PATH=\"$shown/bin:\$PATH\"" ;;
esac
on_path=
case ":$PATH:" in *":$home/bin:"*) on_path=1 ;; esac
printf '\n'
if [ -n "$on_path" ]; then
  :
elif [ -f "$rc" ] && { grep -qsF "$home/bin" "$rc" || grep -qsF "$shown/bin" "$rc"; }; then
  note "$(tilde "$rc") adds miso to PATH: open a new shell to use it"
elif [ -n "$modify_path" ] && ask "Add miso to your PATH in $(tilde "$rc")?" y; then
  mkdir -p "$(dirname "$rc")"
  printf '\n# miso (https://haskell-miso.org)\n%s\n' "$line" >> "$rc"
  done_ "Added these lines to the end of $(tilde "$rc"):"
  printf '\n      # miso (https://haskell-miso.org)\n      %s\n\n' "$line"
  printf '  Open a new shell to use miso, or run the export line in this one.\n'
else
  printf 'Add miso to your PATH (in %s):\n\n    %s\n' "$(tilde "$rc")" "$line"
fi
printf '  %sTo remove miso: miso uninstall (it lists the PATH lines to delete).%s\n' "$dim" "$reset"

# Check the installation, and the tools miso needs to build programs
# (a C compiler, and emcc for the browser).
printf '\n'
say "Checking the installation (miso doctor)"
"$home/bin/miso" doctor || true
printf '\n%sNext:%s\n\n    miso new hello && cd hello && miso dev\n\n' "$bold" "$reset"
