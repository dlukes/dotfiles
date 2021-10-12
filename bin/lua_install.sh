#!/bin/sh

set -eu
prefix="$HOME/.local"
dirname=$(dirname "$0")
. "$dirname/util.sh"

should_update() {
  username="$1"; shift
  repo="$1"; shift

  if [ -d "$repo" ]; then
    cd "$repo"
    git fetch --quiet
    # @{u} is the current branch's upstream
    if [ $(git rev-parse HEAD) = $(git rev-parse @{u}) ]; then
      >&2 echo ">>> $repo: newest version is already installed, aborting."
      return 1
    fi
    git pull
  else
    git clone --depth 1 https://github.com/"$username"/"$repo".git
    cd "$repo"
  fi
}



# ------------------------------------------------------- Install LuaJIT {{{1


repo=LuaJIT
>&2 echo ">>> Installing $repo..."

cd "$prefix"
if should_update LuaJIT "$repo"; then
  patch -p1 <<'EOF'
diff --git a/Makefile b/Makefile
index aa1b84b..8cec688 100644
--- a/Makefile
+++ b/Makefile
@@ -117,6 +117,7 @@ install: $(INSTALL_DEP)
 	@echo "==== Installing LuaJIT $(VERSION) to $(PREFIX) ===="
 	$(MKDIR) $(INSTALL_DIRS)
 	cd src && $(INSTALL_X) $(FILE_T) $(INSTALL_T)
+	$(SYMLINK) $(INSTALL_TNAME) $(INSTALL_TSYM)
 	cd src && test -f $(FILE_A) && $(INSTALL_F) $(FILE_A) $(INSTALL_STATIC) || :
 	$(RM) $(INSTALL_DYN) $(INSTALL_SHORT1) $(INSTALL_SHORT2)
 	cd src && test -f $(FILE_SO) && \
@@ -131,12 +132,6 @@ install: $(INSTALL_DEP)
 	cd src && $(INSTALL_F) $(FILES_INC) $(INSTALL_INC)
 	cd src/jit && $(INSTALL_F) $(FILES_JITLIB) $(INSTALL_JITLIB)
 	@echo "==== Successfully installed LuaJIT $(VERSION) to $(PREFIX) ===="
-	@echo ""
-	@echo "Note: the development releases deliberately do NOT install a symlink for luajit"
-	@echo "You can do this now by running this command (with sudo):"
-	@echo ""
-	@echo "  $(SYMLINK) $(INSTALL_TNAME) $(INSTALL_TSYM)"
-	@echo ""


 uninstall:
EOF

  export MACOSX_DEPLOYMENT_TARGET=11.1
  make PREFIX="$prefix"
  make install PREFIX="$prefix"
  git reset --hard --quiet
  >&2 echo ">>> Installed $repo."
fi



# ----------------------------------------------------- Install LuaRocks {{{1


install_luarocks() {
  username=luarocks
  repo=luarocks
  ur="$username/$repo"
  >&2 echo ">>> Installing $repo..."

  installed=v$(luarocks --version 2>/dev/null | grep -oPm1 '[\d\.]+$' || echo '')
  newest=$(curl -sSLf "https://api.github.com/repos/$ur/tags" | grep -oPm1 'v[\d.]+')
  if [ "$installed" = "$newest" ]; then
    >&2 echo ">>> $repo: newest version is already installed, aborting."
    return
  fi

  tmp=$(mktemp -d)
  cd "$tmp"

  download_url=$(
    curl -sSLf "https://api.github.com/repos/$ur/tags" |
      grep -oPm1 'https://.*?tarball[^"]*'
  )
  curl -sSLf "$download_url" -o luarocks.tgz
  tar xzf luarocks.tgz

  cd "$username"-"$repo"-*
  ./configure --prefix="$prefix" --with-lua="$prefix"
  make
  make install

  cd
  rm -rf "$tmp"
}

install_luarocks



# ------------------------------------------- Install ninja build system {{{1


repo=ninja
if is_macos; then
  os=mac
else
  os=linux
fi
archive="$repo-$os.zip"
>&2 echo ">>> Installing $repo..."

download_url=$(
  curl -sSLf "https://api.github.com/repos/ninja-build/$repo/releases/latest" |
    grep -oPm1 "https://.*?/$archive"
)
cd "$prefix"/bin
curl -sSLfO "$download_url"
unzip -qqo "$archive"
rm "$archive"
>&2 echo ">>> Installed $repo."



# -------------------------------- Install sumneko's Lua language server {{{1


repo=lua-language-server
>&2 echo ">>> Installing $repo..."

cd "$prefix"
if should_update sumneko "$repo"; then
  git submodule update --init --recursive --depth 1

  cd 3rd/luamake
  ./compile/install.sh
  cd ../..
  ./3rd/luamake/luamake rebuild

  # NOTE: Both "bin"s below are intended to refer to the bin directory under the repo's
  # root. There are per-platform subdirs, bin/{Linux,macOS,Windows}, of which just one
  # will exist, depending on which platform you compiled on. We move all binaries from
  # that one existing subdir up one level to bin, so that we can hardcode the path in
  # init.lua and not worry about distinguishing between Linux and macOS.
  #
  # By contrast, we don't symlink the binaries to ~/.local/bin, the command for this
  # particular language server has to be configured manually, so it's no use putting it
  # on the path.
  ln -sft bin "$PWD"/bin/*/*
  >&2 echo ">>> Installed $repo."
fi

# vi: foldmethod=marker
