#!/bin/sh

set -eufx
cd /usr/share/hunspell
sudo curl -fsSLo cs_CZ.dic https://github.com/wooorm/dictionaries/raw/main/dictionaries/cs/index.dic
sudo curl -fsSLo cs_CZ.aff https://github.com/wooorm/dictionaries/raw/main/dictionaries/cs/index.aff
