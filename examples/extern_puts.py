from __monty import extern


@extern("C")
def puts(x: str) -> None:
    ...


def main() -> int:
    puts("Hello, world!")
    return 0
