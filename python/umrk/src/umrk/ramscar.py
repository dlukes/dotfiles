from __future__ import annotations
from typing import cast

import altair as alt
import matplotlib.pyplot as plt
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


def rank_freq_plot(
    data: nltk.FreqDist | pd.DataFrame, x: str = "rank", y: str = "freq"
):
    if isinstance(data, nltk.FreqDist):
        data = pd.DataFrame(data.most_common(1000), columns=["word", "freq"])
    df, x, y = ramscar.prep_df(data, x, y)
    _, r2_exp, _, r2_power = ramscar.fit(df)

    fig, (loglogax, semilogax) = plt.subplots(1, 2)
    loglogax.loglog(x, y, "o", c="C0", data=df)
    loglogax.text(0.1, 0.1, f"R² = {r2_power:.2f}", transform=loglogax.transAxes)
    semilogax.text(0.1, 0.1, f"R² = {r2_exp:.2f}", transform=semilogax.transAxes)
    semilogax.semilogy(x, y, "o", c="C1", data=df)

    fig.suptitle(f"Frekvenční distribuce:")
    loglogax.set_title("Mocninná…")
    semilogax.set_title("… či exponenciální?")
    loglogax.set_xlabel(f"log({x})")
    loglogax.set_ylabel(f"log({y})")
    semilogax.set_xlabel(x)
    semilogax.set_ylabel(f"log({y})")

    return fig, loglogax, semilogax


def wait_time_plot(lemma: str, tagged):
    df = wait_times(lemma, tagged).reset_index()
    params_exp, r2_exp, params_power, r2_power = ramscar.fit(df, x="wait_time")
    df["freq_power_fit"] = power_law(df["wait_time"], *params_power)
    df["freq_exp_fit"] = exponential(df["wait_time"], *params_exp)

    fig, ax = plt.subplots()
    ax.scatter("wait_time", "freq", c="C2", alpha=0.5, label="empirická data", data=df)
    df = df.sort_values("freq_power_fit")
    ax.plot(
        "wait_time",
        "freq_power_fit",
        c="C0",
        label=f"mocninný model (R² = {r2_power:.2f})",
        data=df,
    )
    df = df.sort_values("freq_exp_fit")
    ax.plot(
        "wait_time",
        "freq_exp_fit",
        c="C1",
        label=f"exponenciální model (R² = {r2_exp:.2f})",
        data=df,
    )

    ax.set_xlabel(f"Vzdálenost mezi výskyty lemmatu ‘{lemma}’ (v tokenech)")
    ax.set_ylabel("Frekvence")
    ax.legend()
    ax.set_title(f"Lemma ‘{lemma}’ v korpusu Čapkových textů")

    return fig, loglogax, semilogax


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
