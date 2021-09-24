try:
    from rich import pretty, traceback

    pretty.install()
    traceback.install(show_locals=True)
    del pretty, traceback
except ImportError:
    pass
