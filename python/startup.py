try:
    from rich import pretty

    pretty.install()
    del pretty
except ImportError:
    pass
