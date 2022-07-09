"""Core implementation of import.

This module is NOT meant to be directly imported!
One should use importlib instead as the public-facing version of this module.

This file is based on `cpython/Lib/importlib/_bootstrap.py` but has
been tailored to fit monty's needs more precisely, it is not a full
re-implentation of the CPython import system but an ad-hoc one.

Here lies the implementation of path-based imports working with file loaders ONLY.
There is no support for fancy loaders for zip files, eggs, SO's or other fancy stuff.

If the file looks empty that probably intentional. The interpreter runtime
artificially setattrs this module and its classes with the relevant import magic.
"""


class SourceFileLoader:
    """Loader implementation for source files using the filesystem."""


class PathFinder:
    """Meta path finder for sys.path and package __path__ attributes."""


def _install(sys):
    sys.meta_path.append(PathFinder)
