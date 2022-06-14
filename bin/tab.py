#!/usr/bin/env python3

from pathlib import Path
import regex as re
import sys

from IPython import start_ipython

suffix2reader = {".xlsx": "excel"}

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
    reader = suffix2reader.get(suffix, suffix)
    varname = re.sub(r"^\d+", "", path.stem)
    varname = re.sub(r"\W", "_", varname)
    df = getattr(ps, f"read_{reader}")(path)
    if pandas:
        df = df.convert_dtypes()
    dfs[varname] = df
    print(f"Loaded {path!r} into {varname!r}.")

start_ipython(argv=[], user_ns=dfs)
