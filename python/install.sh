#!/bin/sh

set -e
dirname=$( dirname "$0" )
. "$dirname/util.sh"

>&2 echo "
Worth considering if compiling Python turns out to be a pain: download a pre-compiled
standalone build from <https://github.com/indygreg/python-build-standalone>, extract it
with 'tar xvaf' somewhere where zstd is available (grr), rsync the install subdirectory
to pyenv/versions and rename it according to the version. Pyenv should then be able to
manage it.

Note that you might have to set the TERMINFO_DIRS variable (and possibly other quirks,
see <https://python-build-standalone.readthedocs.io/en/latest/quirks.html>).

Press ENTER to continue with compilation.
"
read __ignored_reply

cat <<EOF >&2
########################################################################
# NOTE: Check out the suggested build environment for your platform    #
# for the deps to install prior to compilation:                        #
#                                                                      #
#   https://github.com/pyenv/pyenv/wiki#suggested-build-environment    #
#                                                                      #
# Here are some quick tips if you want Tkinter and other optional      #
# stdlib extras. E.g. for Solus:                                       #
#                                                                      #
#   sudo eopkg it tk-devel bzip2-devel xz-devel sqlite3-devel          #
#                                                                      #
# Or Ubuntu:                                                           #
#                                                                      #
#   sudo apt build-dep python3-stdlib-extensions                       #
#                                                                      #
# Or on macOS with Homebrew:                                           #
#                                                                      #
#   brew install tcl-tk                                                #
#   export PYTHON_CONFIGURE_OPTS="--with-tcltk-includes=               #
#     '-I/usr/local/opt/tcl-tk/include' --with-tcltk-libs=             #
#     '-L/usr/local/opt/tcl-tk/lib -ltcl8.6 -ltk8.6'"                  #
#                                                                      #
#   (Adapt as needed for different versions, of course.)               #
#                                                                      #
########################################################################
EOF

if is_macos; then
  alias grep=ggrep
  for brew_pkg in openssl readline sqlite3 xz zlib; do
    brew_install_or_upgrade $brew_pkg
  done

  opt=/opt/homebrew/opt
  for lib in sqlite xz zlib; do
    export LDFLAGS="$LDFLAGS -L$opt/$lib/lib"
    export CPPFLAGS="$CPPFLAGS -I$opt/$lib/include"
    # export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$opt/$lib/lib/pkgconfig"
  done
fi

export PYENV_ROOT="$HOME/.local/pyenv"
export PYTHON_CONFIGURE_OPTS=--enable-optimizations
# Cf. also https://github.com/pyenv/pyenv/blob/master/plugins/python-build/README.md#special-environment-variables
# for details on what you can tweak during Python compilation, but apart
# from --enable-optimizations above, there probably isn't anything you
# need. IPv6 is enabled if available and -O3 is the default. And you
# probably don't want --enable-shared unless you really need to link to
# an app which embeds Python, because it makes the interpreter slower
# unless additional precautions are taken, cf. https://pythonspeed.com/articles/faster-python/.
if [ -d "$PYENV_ROOT" ]; then
  git -C "$PYENV_ROOT" pull
else
  git clone https://github.com/pyenv/pyenv.git "$PYENV_ROOT"
fi
ln -sft "$HOME/.local/bin" "$PYENV_ROOT/libexec/pyenv"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

py3_regex='^3\.\d+\.\d+$'
curr_ver=$( pyenv versions --bare | grep -P "$py3_regex" | tail -n1 )
new_ver=$(
  pyenv install --list | tr -d ' ' | grep -P "$py3_regex" | tail -n1
)

pyenv versions
# don't abort entire script if user chooses not to uninstall previous
# version; the subshell is needed on old bash versions (<4) because of
# https://stackoverflow.com/a/68144864
(pyenv uninstall $curr_ver) || true
without_gnubin pyenv install --keep $new_ver

pyenv shell $new_ver
"$dirname"/pip_install.sh
"$dirname"/pdm_install.sh
"$dirname"/umrk_install.sh

# build local copy of docs (only useful on systems with a GUI)
if is_macos || [ -n "$XDG_CURRENT_DESKTOP" ]; then
  src_dir="$PYENV_ROOT/sources"
  doc_dir="$(pyenv prefix)/share/doc"
  cd "$src_dir/$new_ver/Python-$new_ver/Doc"
  make venv
  >&2 echo "Upgrading to latest Sphinx to build docs. Downgrade in case of errors. Press ENTER."
  read __ignored_reply
  . venv/bin/activate
  pip install -U sphinx
  make html
  mkdir -p "$doc_dir"
  mv -T build/html "$doc_dir/python"
  cd -
  rm -rf "$src_dir"
else
  >&2 echo 'Headless system, not building local copy of docs.'
fi

pyenv global $new_ver
