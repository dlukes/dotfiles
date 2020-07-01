#!/bin/bash

dirname=$( dirname "$0" )
. "$dirname/util.sh"

man_dir="$HOME/.local/share/man/man1"
comp_dir="$HOME/.config/fish/completions"
registry="$HOME/.cargo/registry"
mkdir -p "$man_dir" "$comp_dir"

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
  suffix=mac
else
  suffix=linux
fi
rust_analyzer=rust-analyzer-$suffix
download_url=$( curl -sSf https://api.github.com/repos/rust-analyzer/rust-analyzer/releases/latest |
  grep browser_download_url |
  grep -oP "https://.*?$rust_analyzer"
)
curl -sSf -LO $download_url
chmod +x $rust_analyzer
mv $rust_analyzer ~/.local/bin/rust-analyzer

### Exit early?

read -ep ">>> Exit early, without compiling additional utilities? (y/n) " early
if [ "$early" = y ] || [ "$early" = Y ]; then
  exit
fi

### Utils

>&2 echo '>>> Installing general Rust-based utils...'

# Copy asset (typically man page or completions) not automatically
# installed by cargo.
copy_asset() {
  local util="$1"; shift
  local build_dir="$2"; shift
  local target_dir="$3"; shift
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
  ripgrep
  fd-find
  exa
  dust
  hexyl
  xsv
  bat
  ruplacer
  hyperfine
  tokei
)
for util in ${utils[@]}; do
  tmp=$( mktemp -d )
  CARGO_TARGET_DIR="$tmp" cargo install --force $util
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

read -ep ">>> Would you also like to install cargo extensions useful for development? (y/n) " devel
if [ "$devel" = y ] || [ "$devel" = Y ]; then
  extensions+=(
    # managing deps from command line (NOTE: the subcommands are add, rm and
    # upgrade)
    cargo-edit
    # dealing with outdated deps
    cargo-outdated
    # inspecting dep trees (useful for detecting duplicate deps with -d)
    cargo-tree
  )
  cargo install -f ${extensions[@]}
fi

cat <<'EOF' >&2
>>> All done. If you want to modify installed components, profiles are a
handy shortcut:

rustup set profile minimal/default/complete

Remember that you can update Rust with:

rustup update

And you can update cargo-installed binaries with:

cargo install-update -a

Consider running cargo cache -e from time to time, or manually deleting
stuff under ~/.cargo to clean up space.
EOF
