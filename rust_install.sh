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

cargo install ripgrep
cargo install fd-find
cargo install exa

### Cargo extensions

>&2 echo ">>> Installing cargo extensions..."

# keeping track (and pruning) the cache under ~/.cargo
cargo install cargo-cache
# updating global binaries (NOTE: the command is cargo install-update)
cargo install cargo-update

# managing deps from command line (NOTE: the subcommands are add, rm and
# upgrade)
cargo install cargo-edit
# dealing with outdated deps
cargo install cargo-outdated
# inspecting dep trees (useful for detecting duplicate deps with -d)
cargo install cargo-tree

# possibly interesting:
# cargo-readme
# cargo-benchcmp

cat <<EOF >&2
>>> All done. Remember that you can update Rust with:

rustup update

And you can update cargo-installed binaries with:

cargo install-update -a
EOF
