def _():
    """foo"""
    return None

from builtins import int

def __() -> int:
    return 0

def ___(_: int) -> int:
    return _

def ____(_: int, __: int):
    pass
