from __monty import extern


@extern("C")
def puts(x: str) -> None:
    ...


def main() -> int:
    puts("C-like extern definitions are a thing!")
    return 0
