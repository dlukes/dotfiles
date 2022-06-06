#!/bin/sh

set -eu
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh
prefix="$HOME"/.local



# ------------------------------------- Functions for installing without package manager {{{1


install_luajit() {
  repo=LuaJIT
  >&2 echo ">>> Installing $repo..."

  cd "$prefix"
  if should_update luajit $repo $repo; then
    cd "$repo"
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
}


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


install_lua_language_server() {
  username=sumneko
  repo=lua-language-server
  >&2 echo ">>> Installing $repo..."

  lls_prefix="$prefix"/lua-language-server
  rm -rf "$lls_prefix"
  mkdir -p "$lls_prefix"
  cd "$lls_prefix"

  ver=$(github_latest_release_tag_name $username $repo)
  archive=lua-language-server-$ver-linux-x64.tar.gz
  curl -sSLfOJ https://github.com/$username/$repo/releases/download/$ver/$archive
  tar xzf $archive
  rm $archive

  wrapper="$prefix"/bin/$repo
  cat <<EOF >"$wrapper"
#!/bin/sh
exec "$lls_prefix/bin/lua-language-server" "\$@"
EOF
  chmod +x "$wrapper"
}



# --------------------------------------------------------------------------------- Main {{{1


if is_macos; then
  brew_install_or_upgrade luajit-openresty
  brew link --force luajit-openresty
  brew_install_or_upgrade luarocks
  brew_install_or_upgrade lua-language-server
else
  if is_fedora; then
    sudo dnf in -by luajit luarocks
  else
    while true; do
        >&2 read -p 'Install standalone LuaJIT and LuaRocks too? [yes/no]: ' yn
        case "$yn" in
            [Yy]*)
              install_luajit
              install_luarocks
              break
              ;;
            [Nn]*)
              break
              ;;
            *) >&2 echo 'Please answer yes or no.';;
        esac
    done
  fi
  install_lua_language_server
fi

# vi: foldmethod=marker
