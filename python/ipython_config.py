# IPython-specific config. Ipykernel also respects this file, see:
# https://ipython.readthedocs.io/en/stable/config/options/kernel.html
#
# Generate a template with all available options:
#
#   ipython profile create [profilename]
#
# Or see: https://ipython.readthedocs.io/en/stable/config/intro.html
c = get_config()  # type: ignore

# Jedi completion currently doesn't work for objects with custom __dir__ (and/or
# __getattr__?) methods, see: https://github.com/ipython/ipython/issues/11856. This is a
# problem for e.g. Pandas indices (and other features probably too, as Pandas takes
# advantage of Python's dynamic magic quite a lot). NOTE: Setting either Completer or
# IPCompleter seems to work. To check current status however, only use:
#   %config IPCompleter.use_jedi
c.Completer.use_jedi = False

# Check if other matplotlib-inline default settings need to be undone with:
#   %config InlineBackend.print_figure_kwargs
c.InlineBackend.print_figure_kwargs = {"bbox_inches": None}
