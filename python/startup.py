from pathlib import Path
from importlib import import_module


class Importer:
    mod2alias = [("numpy", "np"), ("pandas", "pd"), ("altair", "alt")]

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
    from rich import pretty, traceback

    class RichTracebacker:
        """A convenient toggle for ``show_locals`` in rich tracebacks.

        ``show_locals`` is neat but can unfortunately be very noisy, making it
        impossible to fit the traceback into your terminal history. So don't enable it
        by default.

        """

        def __init__(self):
            self._show_locals = False
            self._traceback = traceback

        def __repr__(self) -> str:
            self._show_locals = not self._show_locals
            self._traceback.install(show_locals=self._show_locals)
            return (
                "Enabled" if self._show_locals else "Disabled"
            ) + " show_locals in rich tracebacks."

        def __str__(self):
            return "toggle show_locals in rich tracebacks"

    sl = RichTracebacker()
    pretty.install()
    traceback.install()
    del pretty, traceback, RichTracebacker
except ImportError:
    pass

print(
    "startup.py:",
    ", ".join(
        f"{key} ({val})"
        for key, val in sorted(globals().items())
        if len(key) < 3 and key.islower()
    ),
)
