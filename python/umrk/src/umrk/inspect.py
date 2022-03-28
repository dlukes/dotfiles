def binary_repr(num: int, num_bytes: int = 1) -> str:
    """Binary representation of num as string.

    This is meant to be equivalent to numpy.binary_repr. Its purpose is to remind you
    that that function exists, and possibly to be available when numpy is not.

    Expected use: in teaching contexts, if you ever again have the misfortune to wander
    into bitwise-operator territory. So that you can at least show what you mean.

    >>> binary_repr(1)
    '00000001'
    >>> binary_repr(-1)
    '11111111'
    >>> binary_repr(~1)
    '11111110'
    >>> binary_repr(1 | ~1) == binary_repr(-1)
    True

    """
    return "".join(f"{b:08b}" for b in num.to_bytes(num_bytes, "big", signed=True))
