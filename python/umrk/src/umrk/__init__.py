# TODO (2022-08-19): Setuptools is currently overhauling the way it enables editable
# installs and it seems there are still some kinks to iron out. With UMRK as just a
# namespace package, the import mechanism gets set up incorrectly, so let's just add an
# __init__.py for the time being.
#
# See also https://setuptools.pypa.io/en/latest/userguide/development_mode.html, which
# says that support for namespace packages not using src-layout is experimental. While
# UMRK does use src-layout, it's entirely possible that support for namespace packages
# in general still needs polishing.
