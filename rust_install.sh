#!/usr/bin/env zsh

### Rustup

if ! command -v rustup >/dev/null 2>&1; then
  >&2 echo ">>> Installing rustup..."
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

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

read -q "DEVEL?>>> Would you also like to install cargo extensions useful for development? (y/n) ";
>&2 echo
if [[ $DEVEL = y || $DEVEL = Y ]]; then
  extensions+=(
    # managing deps from command line (NOTE: the subcommands are add, rm and
    # upgrade)
    cargo-edit
    # dealing with outdated deps
    cargo-outdated
    # inspecting dep trees (useful for detecting duplicate deps with -d)
    cargo-tree
  )
  cargo install -f $extensions
fi

cat <<EOF >&2
>>> All done. If you want to modify installed components, profiles are a
handy shortcut:

rustup set profile minimal/default/complete

Remember that you can update Rust with:

rustup update

And you can update cargo-installed binaries with:

cargo install-update -a
EOF
