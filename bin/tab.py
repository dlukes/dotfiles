#!/usr/bin/env python3

from pathlib import Path
import regex as re
import sys

from IPython import start_ipython

if sys.argv[1] == "pandas":
    import pandas as ps

    pandas = True
    paths = sys.argv[2:]
else:
    import polars as ps

    pandas = False
    paths = sys.argv[1:]

dfs = {}
for path in paths:
    path = Path(path)
    suffix = path.suffix.casefold()
    varname = re.sub(r"^\d+", "", path.stem)
    varname = re.sub(r"\W", "_", varname)
    match suffix:
        case ".tsv" if pandas:
            df = ps.read_table(path)
        case ".tsv":
            df = ps.read_csv(path, sep="\t", quote_char=None)
        case ".xlsx":
            df = ps.read_excel(path)
        case _:
            df = getattr(ps, f"read_{reader}")(path)
    if pandas:
        df = df.convert_dtypes()
    dfs[varname] = df
    print(f"Loaded {path!r} into {varname!r}.")

start_ipython(argv=[], user_ns=dfs)
