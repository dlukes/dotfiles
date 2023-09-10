from __future__ import annotations
from importlib import import_module
from pathlib import Path

# WARN: ipython.config is accessible here, but since IPython loads PYTHONSTARTUP
# relatively late, after the config has already been applied, changing the config here
# has no effect (except for lazily loaded stuff like plotting backends). So IPython
# configuration needs to be done in ipython_config.py.
try:
    ipython = get_ipython()  # type: ignore
    # This is not necessarily true (there might be no frontend at all, even though the
    # kernel is attached to a ZMQ), but close enough.
    is_jupyter = ipython.__class__.__name__ == "ZMQInteractiveShell"
except NameError:
    ipython = None
    is_jupyter = False


class Importer:
    mod2alias = [("numpy", "np"), ("pandas", "pd"), ("altair", "alt"), ("polars", "pl")]

    def __init__(self):
        self._import_module = import_module

    def do_import(self) -> tuple[str, str]:
        imported = {}
        not_imported = []
        for mod, alias in self.mod2alias:
            try:
                imported[alias] = self._import_module(mod)
            except ImportError:
                not_imported.append(mod)
        globals().update(imported)
        return ", ".join(
            f"{mod.__name__} as {alias}" for alias, mod in imported.items()
        ), ", ".join(not_imported)

    def __repr__(self) -> str:
        imported, not_imported = self.do_import()
        if imported and not_imported:
            return f"Imported {imported}; failed to import {not_imported}."
        elif not_imported:
            return f"Failed to import {not_imported}."
        return f"Imported {imported}."

    def __str__(self) -> str:
        return "additional imports"


fd = Importer()
p = Path

del Path, Importer, import_module

try:
    # if is_jupyter:
    #     raise ImportError

    # from rich import pretty, traceback
    # rich.inspect is a good fancy substitute for the help builtin, see:
    # https://textual.textualize.io/blog/2023/07/27/using-rich-inspect-to-interrogate-python-objects/
    from rich import inspect as help

    # class RichTracebacker:
    #     """A convenient toggle for ``show_locals`` in rich tracebacks.

    #     ``show_locals`` is neat but can unfortunately be very noisy, making it
    #     impossible to fit the traceback into your terminal history. So don't enable it
    #     by default.

    #     """

    #     def __init__(self):
    #         self._show_locals = False
    #         self._traceback = traceback

    #     def __repr__(self) -> str:
    #         self._show_locals = not self._show_locals
    #         self._traceback.install(show_locals=self._show_locals)
    #         return (
    #             "Enabled" if self._show_locals else "Disabled"
    #         ) + " show_locals in rich tracebacks."

    #     def __str__(self):
    #         return "toggle show_locals in rich tracebacks"

    # sl = RichTracebacker()
    # pretty.install()
    # traceback.install()
    # del pretty, traceback, RichTracebacker
except ImportError:
    pass

try:
    import regex as re

    re.DEFAULT_VERSION = re.VERSION1
except ImportError:
    pass

# No sense printing startup messages when running as a Jupyter kernel.
if not is_jupyter:
    print(
        "startup.py:",
        "\n".join(
            f"  - {key} ({val})"
            for key, val in sorted(globals().items())
            if len(key) < 3 and key.islower() or key == "help"
        ),
        sep="\n",
    )
    if ipython:
        print("  - %load_ext rich to enable prettier output and tracebacks")
