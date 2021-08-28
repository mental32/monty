from __monty import extern, c_char_p  # type: ignore


@extern("C")
def puts(_: c_char_p) -> int:
    ...


def main() -> int:
    puts("Hello, World")
    return 1
