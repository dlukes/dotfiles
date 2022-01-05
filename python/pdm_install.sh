#!/bin/sh

python3 -m pip install --upgrade pdm
python3 -m pdm completion fish >"$HOME"/.config/fish/completions/pdm.fish
