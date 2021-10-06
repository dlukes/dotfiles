"""Various association measures."""

import math
from typing import Optional, Union, Sequence

from scipy.stats import entropy as _entropy


def entropy(
    dist: Sequence[float],
    other_dist: Optional[Sequence[float]] = None,
    base: Union[int, float] = 2,
    axis: int = 0,
) -> float:
    """Entropy of dist, or relative entropy w.r.t. other_dist.

    Relative entropy is also called Kullback-Leibler divergence and measures how much
    one probability distribution differs from another. **In many cases, this is probably
    what you want in corpus applications**, to make comparisons possible between common
    and less common types. A good default for other_dist is a uniform distribution with
    the same number of items as dist, i.e. `np.ones(len(dist))`.

    See docstring of scipy.stats.entropy for more details.

    """
    return _entropy(dist, other_dist, base, axis)


def mi(px: float, py: float, pxy: float) -> float:
    """Mutual information between events x and y."""
    return math.log2(pxy / px / py)


def mi_score(fx: float, fy: float, fxy: float, N: float) -> float:
    """MI-score between corpus types x and y.

    Arguments: frequency of x, y, and the co-occurrence of x and y, and corpus size.

    Thresholds for what to consider a systematic collocation vary with corpus size. For
    a 100M corpus, an often cited threshold is 7.

    """
    return math.log2(N * fxy / fx / fy)


def t_score(fx: float, fy: float, fxy: float, N: float) -> float:
    return (fxy - fx * fy / N) / math.sqrt(fxy)


def dice(fx: float, fy: float, fxy: float) -> float:
    return 2 * fxy / (fx + fy)


def log_dice(fx: float, fy: float, fxy: float) -> float:
    return 14 + math.log2(dice(fx, fy, fxy))


def log_likelihood() -> float:
    raise NotImplementedError
