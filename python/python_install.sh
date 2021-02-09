#!/bin/sh

set -e
dirname=$( dirname "$0" )
. "$dirname/util.sh"

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
#   sudo eopkg it tk-devel bzip2-devel                                 #
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
fi

export PYENV_ROOT="$HOME/.local/pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
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
eval "$( pyenv init - )"

py3_regex='^3\.\d+\.\d+$'
curr_ver=$( pyenv versions --bare | grep -P "$py3_regex" | tail -n1 )
new_ver=$(
  pyenv install --list | tr -d ' ' | grep -P "$py3_regex" | tail -n1
)
if [ "$new_ver" = "$curr_ver" ]; then
  >&2 echo "Most recent stable Python $curr_ver is already installed."
else
  if [ ! -z "$curr_ver" ]; then
    >&2 echo "Uninstalling old Python $curr_ver"
    pyenv uninstall $curr_ver ||
      >&2 echo "Uninstall aborted; you can run it later manually with 'pyenv uninstall $curr_ver'."
  fi
  >&2 echo "Installing most recent stable Python $new_ver"
  pyenv install $new_ver
  # black's virtualenv is now very probably broken, get rid of it
  rm -rf ~/.local/share/nvim/black
fi

pyenv global system
export POETRY_HOME="$HOME/.local/poetry"
curl -sSLf https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python - --no-modify-path
ln -sft "$HOME/.local/bin" "$POETRY_HOME/bin/poetry"
pyenv global $new_ver

pip3 install --upgrade --upgrade-strategy eager \
  pip \
  wheel
pip3 install --upgrade --upgrade-strategy eager \
  ipython \
  ipdb \
  rich \
  \
  lxml \
  regex \
  requests \
  \
  pandas \
  openpyxl \
  \
  httpie \
  glances \
  \
  jedi-language-server \
  pylint \
  pylint-venv
