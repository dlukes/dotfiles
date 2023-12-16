#!/bin/bash

set -eufo pipefail
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh

man_dir="$HOME/.local/share/man/man1"
comp_dir="$HOME/.config/fish/completions"
registry="$HOME/.cargo/registry"
mkdir -p "$man_dir" "$comp_dir"

if [ -n "${CONDA_PREFIX:-}" ];then
  error "\
Detected Conda env: $CONDA_PREFIX
If it contains a compiler toolchain, it might interfere with Rust compilation. Better
de-activate it first."
  exit 1
fi



# ------------------------------------------------------------------------------- Rustup {{{1


if ! command -v rustup >/dev/null 2>&1; then
  info 'Installing rustup...'
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
else
  info 'Updating rustup...'
  rustup update
fi

rustup completions fish >"$comp_dir/rustup.fish"



# ------------------------------------------------------------------------ Rust analyzer {{{1


info 'Installing rust-analyzer...'

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



# ----------------------------------------------------------------- TODO: Cargo-binstall {{{1
# Can be used to install binaries (if available) instead of compiling from source. As of
# 2023-02-10, I've found it to be unreliable and slow for the tools I use. For both
# ripgrep and fd-find, it has to fall back to QuickInstall, and takes its sweet time to
# do so. Also, for ripgrep, the pre-compiled builds will probably be without PCRE2, but
# I should be able to live with that in exchange for faster installation. So keep an eye
# on the project and possibly use it as the default installation method in the future.


# info 'Installing cargo-binstall...'
# cargo install --locked cargo-binstall



# -------------------------------------------------------------------------------- Utils {{{1


info 'Installing general Rust-based utils...'

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
  eza
  spacedisplay
  hexyl
  # xsv
  bat
  # ruplacer
  hyperfine
  # tokei
  zoxide
  # StyLua installs with a Lua 5.1 compliant parser by default. Other variants,
  # including Luau (typed Lua), can be requested via --features.
  stylua
)
for util in ${utils[@]}; do
  tmp=$( mktemp -d )
  util=$( echo "$util" | sed -e 's/+/ /g')
  CARGO_TARGET_DIR="$tmp" cargo install --locked $util
  copy_asset $util "$tmp" "$man_dir" -regextype egrep -regex ".*/$util-.*/[[:alnum:]]+\.1(\.gz)?"
  copy_asset $util "$tmp" "$comp_dir" -path "*/$util-*" -name "*.fish"
done



# --------------------------------------------------------------------- Cargo extensions {{{1


info 'Installing cargo extensions...'

extensions=(
  # keeping track (and pruning) the cache under ~/.cargo
  cargo-cache
  # updating global binaries (NOTE: the command is cargo install-update); should
  # automatically use cargo-binstall if available
  cargo-update
)
cargo install --locked ${extensions[@]}

info 'All done. Consult your Org-roam on Rustup, Cargo and Rust for maintenance tips.'

# vi: foldmethod=marker
