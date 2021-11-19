from __future__ import annotations
from typing import cast

import altair as alt
import numpy as np
import pandas as pd
from scipy.optimize import curve_fit
from sklearn.metrics import r2_score


def prep_df(data: pd.DataFrame, x: str, y: str) -> tuple[pd.DataFrame, str, str]:
    rank = freq = None
    for colname, col in data.items():
        colname = cast(str, colname).casefold()
        if x in colname:
            rank = col
        elif y in colname:
            freq = col
    rank = np.arange(len(data)) + 1 if rank is None else rank
    assert freq is not None
    x = "Rank" if x == "rank" else x
    y = "Frequency" if y == "freq" else y
    return pd.DataFrame({x: rank, y: freq}), x, y


def plot(data: pd.DataFrame, title: str, x: str = "rank", y: str = "freq") -> alt.Chart:
    """Rank × frequency plot of `data` in both semi-log and log-log scales."""
    data, x, y = prep_df(data, x, y)
    base = alt.Chart(data).mark_line()
    loglog = base.encode(
        alt.X(x, scale=alt.Scale(type="log")),
        alt.Y(y, scale=alt.Scale(type="log")),
    ).properties(title=f"... log-log scale")
    semilog = base.encode(
        alt.X(x),
        alt.Y(
            y,
            scale=alt.Scale(type="log"),
        ),
    ).properties(title="... semi-log scale")
    chart = loglog | semilog
    chart.title = f"Rank × frequency plot of {title} in..."
    return chart


def exponential(x, a, b):
    return a * np.exp(b * x)


def power_law(x, a, b):
    return a * np.power(x, b)


def fit(data: pd.DataFrame, x: str = "rank", y: str = "freq") -> tuple:
    data, x, y = prep_df(data, x, y)

    params_exp = curve_fit(exponential, data[x], data[y], (0, 0))[0]
    # .astype(int) because r2_score doesn't like the nullable Int64 for some reason
    r2_exp = r2_score(data[y].astype(int), exponential(data[x], *params_exp))  # type: ignore
    print("R2 exponential:", r2_exp)

    params_pl = curve_fit(power_law, data[x], data[y], (0, 0))[0]
    r2_pl = r2_score(data[y].astype(int), power_law(data[x], *params_pl))  # type: ignore
    print("R2 power law:", r2_pl)

    return params_exp, r2_exp, params_pl, r2_pl
