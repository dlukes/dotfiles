#!/usr/bin/env python3
"""Helper script to paste note export from https://omnivore.app into Org Roam.

Run note export in Omnivore's UI, which should save the export in the clipboard. Then
run this script to convert to Org and do some post-processing. Use as `:!omnivorg.py`
from within Emacs.

"""

import subprocess as sp
import sys

# Pyperclip is probably installed as a dependency of Pandas.
import pyperclip
import regex as re


def indent_block_quotes(txt: str) -> str:
    lines = []
    in_quote = False
    for line in txt.splitlines():
        line = line.rstrip("\n")
        if "#+begin_quote" in line:
            in_quote = True
        if in_quote:
            line = f"  {line}"
        if "#+end_quote" in line:
            in_quote = False
        lines.append(line)
    return "\n".join(lines)


def main():
    md = pyperclip.paste()
    cmd = "pandoc -f markdown -t org --wrap=none"
    proc = sp.run(cmd.split(), input=md, encoding="utf-8", capture_output=True)
    if proc.returncode != 0:
        sys.stderr.write(f"ERROR running {cmd}:\n")
        sys.stderr.write(proc.stderr)
        sys.exit(proc.returncode)
    org = proc.stdout
    # Strip trailing empty lines in block quotes.
    org = re.sub(r"\n+(?=#\+end_quote)", "\n", org)
    # Indent block quotes.
    org = indent_block_quotes(org)
    print(org)


if __name__ == "__main__":
    main()
