import __monty

assert __monty.arch.os == "linux-gnu"


def print(st: str):
    stdout: int = __monty.libc.fopen(1, "w")
    __monty.libc.fwrite(st, 1, len(st), stdout)
    __monty.libc.fflush(stdout)



def len(st: str) -> int:
    """A dummy len implementation."""
    return 20


def int() -> int:
    return 0
