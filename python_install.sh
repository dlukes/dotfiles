#!/usr/bin/env zsh

set -e
source ${0:a:h}/util.sh

cat <<EOF >&2
########################################################################
# NOTE: If you want Tkinter and other optional stdlib extras, you may  #
# have to first install some dependencies. E.g. for Solus:             #
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

export PYENV_ROOT=$HOME/.local/pyenv
export PATH=$PYENV_ROOT/bin:$PATH
if [[ -d $PYENV_ROOT ]]; then
  git -C $PYENV_ROOT pull
else
  git clone https://github.com/pyenv/pyenv.git $PYENV_ROOT
fi
eval "$(pyenv init -)"

py3_regex='^3\.\d+\.\d+$'
curr_ver=$( pyenv versions --bare | grep -P $py3_regex | tail -n1 )
new_ver=$(
  pyenv install --list | tr -d ' ' | grep -P $py3_regex | tail -n1
)
if [[ $new_ver == $curr_ver ]]; then
  >&2 echo "Most recent stable Python $curr_ver is already installed."
else
  if [[ ! -z $curr_ver ]]; then
    >&2 echo "Uninstalling old Python $curr_ver"
    pyenv uninstall $curr_ver
  fi
  >&2 echo "Installing most recent stable Python $new_ver"
  pyenv install $new_ver
fi

pyenv global system
curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python
pyenv global $new_ver

pip3 install $user --upgrade --upgrade-strategy eager \
  ipython \
  ipdb \
  \
  lxml \
  regex \
  requests \
  dill \
  trio \
  asks \
  \
  numpy \
  pandas \
  openpyxl \
  \
  matplotlib \
  seaborn \
  \
  corpy \
  \
  httpie \
  \
  'python-language-server[rope,pyflakes]' \
  pyls-black \
  \
  pip \
  pipenv
