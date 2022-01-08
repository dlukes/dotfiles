import numpy as np


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
