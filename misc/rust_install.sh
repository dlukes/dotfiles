#!/bin/bash

set -eufo pipefail
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh

man_dir="$HOME/.local/share/man/man1"
comp_dir="$HOME/.config/fish/completions"
registry="$HOME/.cargo/registry"
mkdir -p "$man_dir" "$comp_dir"

>&2 echo '>>> HINT: Any options are passed on to the cargo install calls (e.g. --force).'

### Rustup

if ! command -v rustup >/dev/null 2>&1; then
  >&2 echo '>>> Installing rustup...'
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
else
  >&2 echo '>>> Updating rustup...'
  rustup update
fi

rustup completions fish >"$comp_dir/rustup.fish"

### Rust analyzer

>&2 echo '>>> Installing rust-analyzer...'

if is_macos; then
  suffix=aarch64-apple-darwin
else
  suffix=x86_64-unknown-linux-gnu
fi

rust_analyzer=rust-analyzer-$suffix
rust_analyzer_gz=$rust_analyzer.gz
latest_tag=$(github_latest_release_tag_name rust-analyzer rust-analyzer)
download_url=https://github.com/rust-lang/rust-analyzer/releases/download/$latest_tag/$rust_analyzer_gz
curl -sSf -LO $download_url
gunzip $rust_analyzer_gz
chmod +x $rust_analyzer
mv $rust_analyzer ~/.local/bin/rust-analyzer

### Utils

>&2 echo '>>> Installing general Rust-based utils...'

# Copy asset (typically man page or completions) not automatically
# installed by cargo.
copy_asset() {
  local util="$1"; shift
  local build_dir="$1"; shift
  local target_dir="$1"; shift
  local find_args=( "$@" )

  local asset=$( find "$build_dir" "${find_args[@]}" | sort -Vr | head -n1 )
  if [ -z "$asset" ]; then
    asset=$( find "$registry" "${find_args[@]}" | sort -Vr | head -n1 )
  fi

  if [ ! -z "$asset" ]; then
    local bname=$( basename "$asset" )
    local target="${bname/completions/$util}"
    cp "$asset" "$target_dir/$target"
  fi
}

utils=(
  ripgrep+--features=pcre2
  fd-find
  exa
  # Probably use broot, i.e. br -w, instead, which is interactive?
  # du-dust
  hexyl
  # xsv
  bat
  # ruplacer
  hyperfine
  # tokei
  broot
  zoxide
)
for util in ${utils[@]}; do
  tmp=$( mktemp -d )
  util=$( echo "$util" | sed -e 's/+/ /g')
  CARGO_TARGET_DIR="$tmp" cargo install --locked "$@" $util
  copy_asset $util "$tmp" "$man_dir" -regextype egrep -regex ".*/$util-.*/[[:alnum:]]+\.1(\.gz)?"
  copy_asset $util "$tmp" "$comp_dir" -path "*/$util-*" -name "*.fish"
done

### Cargo extensions

>&2 echo '>>> Installing cargo extensions...'

extensions=(
  # keeping track (and pruning) the cache under ~/.cargo
  cargo-cache
  # updating global binaries (NOTE: the command is cargo install-update)
  cargo-update
)
cargo install --locked "$@" ${extensions[@]}

>&2 echo '>>> All done. Consult your personal notes on Rustup, Cargo and Rust for maintenance tips.'
