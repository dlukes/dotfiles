from __future__ import annotations
import typing as t

import numpy as np
import numpy.typing as npt


def hz2st(f: float, ref: float) -> float:
    """Convert frequency f in Hz to ST, relative to frequency ref in Hz.

    >>> hz2st(400, 200)
    12.0
    >>> hz2st(200, 400)
    -12.0

    """
    return 12 / np.log(2) * np.log(f / ref)


def st2hz(st: float, ref: float) -> float:
    """Convert interval st in ST to Hz, relative to frequency ref in Hz.

    >>> st2hz(12, 200)
    400.0
    >>> st2hz(-12, 400)
    200.0

    """
    return np.exp(st * np.log(2) / 12) * ref


def npvi(x: t.Sequence[float] | npt.NDArray[np.floating[t.Any]]) -> float:
    """Compute normalized pairwise variability index (nPVI) of array-like x.

    >>> npvi([4, 6] * 5)
    0.4
    >>> round(npvi(np.linspace(2, 8, 10)), 3)
    0.154

    """
    # The cast is required so that Pyright doesn't complain about the subtraction below.
    # Dtype type annotations and inference is tricky, unfortunately. See this numpy
    # issue <https://github.com/numpy/numpy/issues/20568> for details -- some cases have
    # been recently improved (e.g. the inference of np.zeros), but not the present one.
    a = t.cast(npt.NDArray[np.floating[t.Any]], np.array(x))
    next_a = a[1:]
    a = a[:-1]
    return np.abs(2 * (a - next_a) / (a + next_a)).sum() / len(a)
