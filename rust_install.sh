#!/usr/bin/env zsh

### Rustup

if ! command -v rustup >/dev/null 2>&1; then
  >&2 echo ">>> Installing rustup..."
  curl https://sh.rustup.rs -sSf | sh
fi

### Rustup additional components

>&2 echo ">>> Adding rustup components..."

rustup component add rls-preview
rustup component add rustfmt-preview
rustup component add clippy-preview

### Utils

>&2 echo ">>> Installing general Rust-based utils..."

utils=(
  ripgrep
  fd-find
  exa
  # 12 largest dirs in $DIR: sn sort $DIR -n12
  tin-summer
  hexyl
  xsv
  bat
  ruplacer
)
cargo install -f $utils

### Cargo extensions

>&2 echo ">>> Installing cargo extensions..."

extensions=(
  # keeping track (and pruning) the cache under ~/.cargo
  cargo-cache
  # updating global binaries (NOTE: the command is cargo install-update)
  cargo-update
)

if read -q "DEVEL?>>> Would you also like to install cargo extensions useful for development?"; then
  extensions+=(
    # managing deps from command line (NOTE: the subcommands are add, rm and
    # upgrade)
    cargo-edit
    # dealing with outdated deps
    cargo-outdated
    # inspecting dep trees (useful for detecting duplicate deps with -d)
    cargo-tree
  )
fi
>&2 echo
cargo install -f $extensions

cat <<EOF >&2
>>> All done. Remember that you can update Rust with:

rustup update

And you can update cargo-installed binaries with:

cargo install-update -a
EOF
