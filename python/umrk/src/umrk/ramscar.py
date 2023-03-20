from __future__ import annotations
import collections
import typing as t

import altair as alt
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.optimize import curve_fit
from sklearn.metrics import r2_score

T = t.TypeVar("T")
PLOT_TITLE = "Rank × frequency plot of {} in…"
SUBPLOT_TITLES = {"power_law": "… log-log scale", "exponential": "… semi-log scale"}

FreqDist = dict[t.Any, int]


def ensure_rank(
    data: pd.DataFrame | FreqDist,
    *,
    rank_from: str = "FREQ",
    rank_to: str = "RANK",
) -> pd.DataFrame:
    if not isinstance(data, pd.DataFrame):
        data = pd.DataFrame.from_dict(
            data, orient="index", columns=["FREQ"]
        ).reset_index(names="TYPE")
    elif rank_to in data.columns:
        return data
    data[rank_to] = data[rank_from].rank(ascending=False, method="first")
    return data


def wait_times(type_: T, tokens: t.Iterable[T]) -> pd.DataFrame:
    wait_freq_dist = collections.Counter()
    waiting_from = 0
    for i, tok in enumerate(tokens):
        if tok == type_:
            wait_freq_dist[i - waiting_from] += 1
            waiting_from = i
    return ensure_rank(wait_freq_dist)


def plot(
    data: pd.DataFrame | FreqDist,
    title: str,
    *,
    x: str = "RANK",
    y: str = "FREQ",
    mosaic: list[list[str]] = [["fits", "power_law"], ["fits", "exponential"]],
) -> tuple[pd.DataFrame, dict[str, Model], plt.figure.Figure, dict[str, plt.Axes]]:
    """Compare freq. dist. f(x) = y in `data` to power law and exponential models.

    The figure can contain up to three kinds of subplots, as specified by the following
    keys in `mosaic`:

    - `"fits"`: Scatter plot on linear axes, along with values predicted by power law
      and exponential models.
    - `"power_law"`: Scatter plot on log-log axes.
    - `"exponential"`: Scatter plot on semi-log (y) axes.

    >>> from collections import Counter
    >>> c = Counter("ababacbcbcababcbacbabbabababbaaaababacbacbabbcbabacbbcbbbabababbb")
    >>> def show(*args): plt.show()
    >>> show(plot(c, "test 1"))
    >>> show(plot(c, "test 2", mosaic=[["fits"]]))
    >>> show(plot(c, "test 3", mosaic=[["power_law", "exponential"]]))
    >>> show(plot(c, "test 4", mosaic=[["power_law", "exponential"], ["fits", "fits"]]))

    """
    data = ensure_rank(data, rank_from=y, rank_to=x)
    models = fit(data, x=x, y=y)
    cs = {"power_law": "C0", "exponential": "C1"}
    fig, axd = plt.subplot_mosaic(mosaic)
    fig.suptitle(PLOT_TITLE.format(title))

    if "fits" in axd:
        axd["fits"].scatter(x, y, c="C2", alpha=0.5, label="Empirical", data=data)
        axd["fits"].set_xlabel(x)
        axd["fits"].set_ylabel(y)

    for k, m in models.items():
        if k in axd:
            plot = axd[k].loglog if k == "power_law" else axd[k].semilogy
            plot(x, y, "o", c=cs[k], data=data)
            axd[k].set_title(SUBPLOT_TITLES[k])
            axd[k].set_xlabel(f"log({x})")
            axd[k].set_ylabel(f"log({y})" if k == "power_law" else y)
            # If showing fits, R² will be shown in the legend.
            if "fits" not in axd:
                axd[k].text(0.1, 0.1, f"R² = {m.r2:.2f}", transform=axd[k].transAxes)
        if "fits" in axd:
            yf = f"{y}_{k.upper()}"
            data[yf] = m.predict(data[x], *m.params)
            label = f"{k.capitalize().replace('_', ' ')} model (R² = {m.r2:.2f})"
            axd["fits"].plot(x, yf, c=cs[k], label=label, data=data.sort_values(y))

    if "fits" in axd:
        axd["fits"].legend()

    return data, models, fig, axd


def plot_altair(
    data: pd.DataFrame | FreqDist,
    title: str,
    *,
    x: str = "RANK",
    y: str = "FREQ",
) -> alt.Chart:
    data = ensure_rank(data, rank_from=y, rank_to=x)
    base = alt.Chart(data).mark_line()
    loglog = base.encode(
        alt.X(x, scale=alt.Scale(type="log")),
        alt.Y(y, scale=alt.Scale(type="log")),
    ).properties(title=SUBPLOT_TITLES["power_law"])
    semilog = base.encode(
        alt.X(x),
        alt.Y(
            y,
            scale=alt.Scale(type="log"),
        ),
    ).properties(title=SUBPLOT_TITLES["exponential"])
    chart = loglog | semilog
    chart.title = PLOT_TITLE.format(title)
    return chart


def exponential(x, a, b):
    return a * np.exp(b * x)


def power_law(x, a, b):
    return a * np.power(x, b)


class Model(t.NamedTuple):
    name: str
    params: tuple
    r2: float
    predict: t.Callable


def fit(
    data: pd.DataFrame | FreqDist, *, x: str = "RANK", y: str = "FREQ"
) -> dict[str, Model]:
    data = ensure_rank(data, rank_from=y, rank_to=x)
    ans = {}
    for func in (power_law, exponential):
        params = curve_fit(func, data[x], data[y], (0, 0))[0]
        # .astype(int) because r2_score doesn't like the nullable Int64 for some reason
        r2 = r2_score(data[y].astype(int), func(data[x], *params))
        assert isinstance(r2, float)
        ans[func.__name__] = Model(func.__name__, params, r2, func)
    return ans
